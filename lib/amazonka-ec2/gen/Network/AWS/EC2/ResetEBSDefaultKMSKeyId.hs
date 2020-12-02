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
-- Module      : Network.AWS.EC2.ResetEBSDefaultKMSKeyId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets the default customer master key (CMK) for EBS encryption for your account in this Region to the AWS managed CMK for EBS.
--
--
-- After resetting the default CMK to the AWS managed CMK, you can continue to encrypt by a customer managed CMK by specifying it when you create the volume. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.ResetEBSDefaultKMSKeyId
  ( -- * Creating a Request
    resetEBSDefaultKMSKeyId,
    ResetEBSDefaultKMSKeyId,

    -- * Request Lenses
    redkkiDryRun,

    -- * Destructuring the Response
    resetEBSDefaultKMSKeyIdResponse,
    ResetEBSDefaultKMSKeyIdResponse,

    -- * Response Lenses
    redkkirsKMSKeyId,
    redkkirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'resetEBSDefaultKMSKeyId' smart constructor.
newtype ResetEBSDefaultKMSKeyId = ResetEBSDefaultKMSKeyId'
  { _redkkiDryRun ::
      Maybe Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResetEBSDefaultKMSKeyId' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'redkkiDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
resetEBSDefaultKMSKeyId ::
  ResetEBSDefaultKMSKeyId
resetEBSDefaultKMSKeyId =
  ResetEBSDefaultKMSKeyId' {_redkkiDryRun = Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
redkkiDryRun :: Lens' ResetEBSDefaultKMSKeyId (Maybe Bool)
redkkiDryRun = lens _redkkiDryRun (\s a -> s {_redkkiDryRun = a})

instance AWSRequest ResetEBSDefaultKMSKeyId where
  type Rs ResetEBSDefaultKMSKeyId = ResetEBSDefaultKMSKeyIdResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          ResetEBSDefaultKMSKeyIdResponse'
            <$> (x .@? "kmsKeyId") <*> (pure (fromEnum s))
      )

instance Hashable ResetEBSDefaultKMSKeyId

instance NFData ResetEBSDefaultKMSKeyId

instance ToHeaders ResetEBSDefaultKMSKeyId where
  toHeaders = const mempty

instance ToPath ResetEBSDefaultKMSKeyId where
  toPath = const "/"

instance ToQuery ResetEBSDefaultKMSKeyId where
  toQuery ResetEBSDefaultKMSKeyId' {..} =
    mconcat
      [ "Action" =: ("ResetEbsDefaultKmsKeyId" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "DryRun" =: _redkkiDryRun
      ]

-- | /See:/ 'resetEBSDefaultKMSKeyIdResponse' smart constructor.
data ResetEBSDefaultKMSKeyIdResponse = ResetEBSDefaultKMSKeyIdResponse'
  { _redkkirsKMSKeyId ::
      !(Maybe Text),
    _redkkirsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResetEBSDefaultKMSKeyIdResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'redkkirsKMSKeyId' - The Amazon Resource Name (ARN) of the default CMK for EBS encryption by default.
--
-- * 'redkkirsResponseStatus' - -- | The response status code.
resetEBSDefaultKMSKeyIdResponse ::
  -- | 'redkkirsResponseStatus'
  Int ->
  ResetEBSDefaultKMSKeyIdResponse
resetEBSDefaultKMSKeyIdResponse pResponseStatus_ =
  ResetEBSDefaultKMSKeyIdResponse'
    { _redkkirsKMSKeyId = Nothing,
      _redkkirsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the default CMK for EBS encryption by default.
redkkirsKMSKeyId :: Lens' ResetEBSDefaultKMSKeyIdResponse (Maybe Text)
redkkirsKMSKeyId = lens _redkkirsKMSKeyId (\s a -> s {_redkkirsKMSKeyId = a})

-- | -- | The response status code.
redkkirsResponseStatus :: Lens' ResetEBSDefaultKMSKeyIdResponse Int
redkkirsResponseStatus = lens _redkkirsResponseStatus (\s a -> s {_redkkirsResponseStatus = a})

instance NFData ResetEBSDefaultKMSKeyIdResponse
