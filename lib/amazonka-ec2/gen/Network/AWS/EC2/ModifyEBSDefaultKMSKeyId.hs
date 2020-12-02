{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyEBSDefaultKMSKeyId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the default customer master key (CMK) for EBS encryption by default for your account in this Region.
--
--
-- AWS creates a unique AWS managed CMK in each Region for use with encryption by default. If you change the default CMK to a symmetric customer managed CMK, it is used instead of the AWS managed CMK. To reset the default CMK to the AWS managed CMK for EBS, use 'ResetEbsDefaultKmsKeyId' . Amazon EBS does not support asymmetric CMKs.
--
-- If you delete or disable the customer managed CMK that you specified for use with encryption by default, your instances will fail to launch.
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.ModifyEBSDefaultKMSKeyId
  ( -- * Creating a Request
    modifyEBSDefaultKMSKeyId,
    ModifyEBSDefaultKMSKeyId,

    -- * Request Lenses
    medkkiDryRun,
    medkkiKMSKeyId,

    -- * Destructuring the Response
    modifyEBSDefaultKMSKeyIdResponse,
    ModifyEBSDefaultKMSKeyIdResponse,

    -- * Response Lenses
    medkkirsKMSKeyId,
    medkkirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyEBSDefaultKMSKeyId' smart constructor.
data ModifyEBSDefaultKMSKeyId = ModifyEBSDefaultKMSKeyId'
  { _medkkiDryRun ::
      !(Maybe Bool),
    _medkkiKMSKeyId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyEBSDefaultKMSKeyId' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'medkkiDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'medkkiKMSKeyId' - The identifier of the AWS Key Management Service (AWS KMS) customer master key (CMK) to use for Amazon EBS encryption. If this parameter is not specified, your AWS managed CMK for EBS is used. If @KmsKeyId@ is specified, the encrypted state must be @true@ . You can specify the CMK using any of the following:     * Key ID. For example, 1234abcd-12ab-34cd-56ef-1234567890ab.     * Key alias. For example, alias/ExampleAlias.     * Key ARN. For example, arn:aws:kms:us-east-1:012345678910:key/1234abcd-12ab-34cd-56ef-1234567890ab.     * Alias ARN. For example, arn:aws:kms:us-east-1:012345678910:alias/ExampleAlias. AWS authenticates the CMK asynchronously. Therefore, if you specify an ID, alias, or ARN that is not valid, the action can appear to complete, but eventually fails. Amazon EBS does not support asymmetric CMKs.
modifyEBSDefaultKMSKeyId ::
  -- | 'medkkiKMSKeyId'
  Text ->
  ModifyEBSDefaultKMSKeyId
modifyEBSDefaultKMSKeyId pKMSKeyId_ =
  ModifyEBSDefaultKMSKeyId'
    { _medkkiDryRun = Nothing,
      _medkkiKMSKeyId = pKMSKeyId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
medkkiDryRun :: Lens' ModifyEBSDefaultKMSKeyId (Maybe Bool)
medkkiDryRun = lens _medkkiDryRun (\s a -> s {_medkkiDryRun = a})

-- | The identifier of the AWS Key Management Service (AWS KMS) customer master key (CMK) to use for Amazon EBS encryption. If this parameter is not specified, your AWS managed CMK for EBS is used. If @KmsKeyId@ is specified, the encrypted state must be @true@ . You can specify the CMK using any of the following:     * Key ID. For example, 1234abcd-12ab-34cd-56ef-1234567890ab.     * Key alias. For example, alias/ExampleAlias.     * Key ARN. For example, arn:aws:kms:us-east-1:012345678910:key/1234abcd-12ab-34cd-56ef-1234567890ab.     * Alias ARN. For example, arn:aws:kms:us-east-1:012345678910:alias/ExampleAlias. AWS authenticates the CMK asynchronously. Therefore, if you specify an ID, alias, or ARN that is not valid, the action can appear to complete, but eventually fails. Amazon EBS does not support asymmetric CMKs.
medkkiKMSKeyId :: Lens' ModifyEBSDefaultKMSKeyId Text
medkkiKMSKeyId = lens _medkkiKMSKeyId (\s a -> s {_medkkiKMSKeyId = a})

instance AWSRequest ModifyEBSDefaultKMSKeyId where
  type Rs ModifyEBSDefaultKMSKeyId = ModifyEBSDefaultKMSKeyIdResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          ModifyEBSDefaultKMSKeyIdResponse'
            <$> (x .@? "kmsKeyId") <*> (pure (fromEnum s))
      )

instance Hashable ModifyEBSDefaultKMSKeyId

instance NFData ModifyEBSDefaultKMSKeyId

instance ToHeaders ModifyEBSDefaultKMSKeyId where
  toHeaders = const mempty

instance ToPath ModifyEBSDefaultKMSKeyId where
  toPath = const "/"

instance ToQuery ModifyEBSDefaultKMSKeyId where
  toQuery ModifyEBSDefaultKMSKeyId' {..} =
    mconcat
      [ "Action" =: ("ModifyEbsDefaultKmsKeyId" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "DryRun" =: _medkkiDryRun,
        "KmsKeyId" =: _medkkiKMSKeyId
      ]

-- | /See:/ 'modifyEBSDefaultKMSKeyIdResponse' smart constructor.
data ModifyEBSDefaultKMSKeyIdResponse = ModifyEBSDefaultKMSKeyIdResponse'
  { _medkkirsKMSKeyId ::
      !(Maybe Text),
    _medkkirsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyEBSDefaultKMSKeyIdResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'medkkirsKMSKeyId' - The Amazon Resource Name (ARN) of the default CMK for encryption by default.
--
-- * 'medkkirsResponseStatus' - -- | The response status code.
modifyEBSDefaultKMSKeyIdResponse ::
  -- | 'medkkirsResponseStatus'
  Int ->
  ModifyEBSDefaultKMSKeyIdResponse
modifyEBSDefaultKMSKeyIdResponse pResponseStatus_ =
  ModifyEBSDefaultKMSKeyIdResponse'
    { _medkkirsKMSKeyId = Nothing,
      _medkkirsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the default CMK for encryption by default.
medkkirsKMSKeyId :: Lens' ModifyEBSDefaultKMSKeyIdResponse (Maybe Text)
medkkirsKMSKeyId = lens _medkkirsKMSKeyId (\s a -> s {_medkkirsKMSKeyId = a})

-- | -- | The response status code.
medkkirsResponseStatus :: Lens' ModifyEBSDefaultKMSKeyIdResponse Int
medkkirsResponseStatus = lens _medkkirsResponseStatus (\s a -> s {_medkkirsResponseStatus = a})

instance NFData ModifyEBSDefaultKMSKeyIdResponse
