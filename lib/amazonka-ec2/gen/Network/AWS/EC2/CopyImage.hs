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
-- Module      : Network.AWS.EC2.CopyImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates the copy of an AMI from the specified source Region to the current Region. You specify the destination Region by using its endpoint when making the request.
--
--
-- Copies of encrypted backing snapshots for the AMI are encrypted. Copies of unencrypted backing snapshots remain unencrypted, unless you set @Encrypted@ during the copy operation. You cannot create an unencrypted copy of an encrypted backing snapshot.
--
-- For more information about the prerequisites and limits when copying an AMI, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/CopyingAMIs.html Copying an AMI> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.CopyImage
  ( -- * Creating a Request
    copyImage,
    CopyImage,

    -- * Request Lenses
    ciClientToken,
    ciEncrypted,
    ciKMSKeyId,
    ciDescription,
    ciDryRun,
    ciName,
    ciSourceImageId,
    ciSourceRegion,

    -- * Destructuring the Response
    copyImageResponse,
    CopyImageResponse,

    -- * Response Lenses
    ciirsImageId,
    ciirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for CopyImage.
--
--
--
-- /See:/ 'copyImage' smart constructor.
data CopyImage = CopyImage'
  { _ciClientToken :: !(Maybe Text),
    _ciEncrypted :: !(Maybe Bool),
    _ciKMSKeyId :: !(Maybe Text),
    _ciDescription :: !(Maybe Text),
    _ciDryRun :: !(Maybe Bool),
    _ciName :: !Text,
    _ciSourceImageId :: !Text,
    _ciSourceRegion :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CopyImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciClientToken' - Unique, case-sensitive identifier you provide to ensure idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'ciEncrypted' - Specifies whether the destination snapshots of the copied image should be encrypted. You can encrypt a copy of an unencrypted snapshot, but you cannot create an unencrypted copy of an encrypted snapshot. The default CMK for EBS is used unless you specify a non-default AWS Key Management Service (AWS KMS) CMK using @KmsKeyId@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'ciKMSKeyId' - The identifier of the symmetric AWS Key Management Service (AWS KMS) customer master key (CMK) to use when creating encrypted volumes. If this parameter is not specified, your AWS managed CMK for EBS is used. If you specify a CMK, you must also set the encrypted state to @true@ . You can specify a CMK using any of the following:     * Key ID. For example, 1234abcd-12ab-34cd-56ef-1234567890ab.     * Key alias. For example, alias/ExampleAlias.     * Key ARN. For example, arn:aws:kms:us-east-1:012345678910:key/1234abcd-12ab-34cd-56ef-1234567890ab.     * Alias ARN. For example, arn:aws:kms:us-east-1:012345678910:alias/ExampleAlias. AWS authenticates the CMK asynchronously. Therefore, if you specify an identifier that is not valid, the action can appear to complete, but eventually fails. The specified CMK must exist in the destination Region. Amazon EBS does not support asymmetric CMKs.
--
-- * 'ciDescription' - A description for the new AMI in the destination Region.
--
-- * 'ciDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'ciName' - The name of the new AMI in the destination Region.
--
-- * 'ciSourceImageId' - The ID of the AMI to copy.
--
-- * 'ciSourceRegion' - The name of the Region that contains the AMI to copy.
copyImage ::
  -- | 'ciName'
  Text ->
  -- | 'ciSourceImageId'
  Text ->
  -- | 'ciSourceRegion'
  Text ->
  CopyImage
copyImage pName_ pSourceImageId_ pSourceRegion_ =
  CopyImage'
    { _ciClientToken = Nothing,
      _ciEncrypted = Nothing,
      _ciKMSKeyId = Nothing,
      _ciDescription = Nothing,
      _ciDryRun = Nothing,
      _ciName = pName_,
      _ciSourceImageId = pSourceImageId_,
      _ciSourceRegion = pSourceRegion_
    }

-- | Unique, case-sensitive identifier you provide to ensure idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> in the /Amazon Elastic Compute Cloud User Guide/ .
ciClientToken :: Lens' CopyImage (Maybe Text)
ciClientToken = lens _ciClientToken (\s a -> s {_ciClientToken = a})

-- | Specifies whether the destination snapshots of the copied image should be encrypted. You can encrypt a copy of an unencrypted snapshot, but you cannot create an unencrypted copy of an encrypted snapshot. The default CMK for EBS is used unless you specify a non-default AWS Key Management Service (AWS KMS) CMK using @KmsKeyId@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
ciEncrypted :: Lens' CopyImage (Maybe Bool)
ciEncrypted = lens _ciEncrypted (\s a -> s {_ciEncrypted = a})

-- | The identifier of the symmetric AWS Key Management Service (AWS KMS) customer master key (CMK) to use when creating encrypted volumes. If this parameter is not specified, your AWS managed CMK for EBS is used. If you specify a CMK, you must also set the encrypted state to @true@ . You can specify a CMK using any of the following:     * Key ID. For example, 1234abcd-12ab-34cd-56ef-1234567890ab.     * Key alias. For example, alias/ExampleAlias.     * Key ARN. For example, arn:aws:kms:us-east-1:012345678910:key/1234abcd-12ab-34cd-56ef-1234567890ab.     * Alias ARN. For example, arn:aws:kms:us-east-1:012345678910:alias/ExampleAlias. AWS authenticates the CMK asynchronously. Therefore, if you specify an identifier that is not valid, the action can appear to complete, but eventually fails. The specified CMK must exist in the destination Region. Amazon EBS does not support asymmetric CMKs.
ciKMSKeyId :: Lens' CopyImage (Maybe Text)
ciKMSKeyId = lens _ciKMSKeyId (\s a -> s {_ciKMSKeyId = a})

-- | A description for the new AMI in the destination Region.
ciDescription :: Lens' CopyImage (Maybe Text)
ciDescription = lens _ciDescription (\s a -> s {_ciDescription = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
ciDryRun :: Lens' CopyImage (Maybe Bool)
ciDryRun = lens _ciDryRun (\s a -> s {_ciDryRun = a})

-- | The name of the new AMI in the destination Region.
ciName :: Lens' CopyImage Text
ciName = lens _ciName (\s a -> s {_ciName = a})

-- | The ID of the AMI to copy.
ciSourceImageId :: Lens' CopyImage Text
ciSourceImageId = lens _ciSourceImageId (\s a -> s {_ciSourceImageId = a})

-- | The name of the Region that contains the AMI to copy.
ciSourceRegion :: Lens' CopyImage Text
ciSourceRegion = lens _ciSourceRegion (\s a -> s {_ciSourceRegion = a})

instance AWSRequest CopyImage where
  type Rs CopyImage = CopyImageResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          CopyImageResponse' <$> (x .@? "imageId") <*> (pure (fromEnum s))
      )

instance Hashable CopyImage

instance NFData CopyImage

instance ToHeaders CopyImage where
  toHeaders = const mempty

instance ToPath CopyImage where
  toPath = const "/"

instance ToQuery CopyImage where
  toQuery CopyImage' {..} =
    mconcat
      [ "Action" =: ("CopyImage" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "ClientToken" =: _ciClientToken,
        "Encrypted" =: _ciEncrypted,
        "KmsKeyId" =: _ciKMSKeyId,
        "Description" =: _ciDescription,
        "DryRun" =: _ciDryRun,
        "Name" =: _ciName,
        "SourceImageId" =: _ciSourceImageId,
        "SourceRegion" =: _ciSourceRegion
      ]

-- | Contains the output of CopyImage.
--
--
--
-- /See:/ 'copyImageResponse' smart constructor.
data CopyImageResponse = CopyImageResponse'
  { _ciirsImageId ::
      !(Maybe Text),
    _ciirsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CopyImageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciirsImageId' - The ID of the new AMI.
--
-- * 'ciirsResponseStatus' - -- | The response status code.
copyImageResponse ::
  -- | 'ciirsResponseStatus'
  Int ->
  CopyImageResponse
copyImageResponse pResponseStatus_ =
  CopyImageResponse'
    { _ciirsImageId = Nothing,
      _ciirsResponseStatus = pResponseStatus_
    }

-- | The ID of the new AMI.
ciirsImageId :: Lens' CopyImageResponse (Maybe Text)
ciirsImageId = lens _ciirsImageId (\s a -> s {_ciirsImageId = a})

-- | -- | The response status code.
ciirsResponseStatus :: Lens' CopyImageResponse Int
ciirsResponseStatus = lens _ciirsResponseStatus (\s a -> s {_ciirsResponseStatus = a})

instance NFData CopyImageResponse
