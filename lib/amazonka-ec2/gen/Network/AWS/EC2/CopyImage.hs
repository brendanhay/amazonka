{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CopyImage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates the copy of an AMI from the specified source region to the current region. You specify the destination region by using its endpoint when making the request.
--
--
-- For more information about the prerequisites and limits when copying an AMI, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/CopyingAMIs.html Copying an AMI> in the /Amazon Elastic Compute Cloud User Guide/ .
--
module Network.AWS.EC2.CopyImage
    (
    -- * Creating a Request
      copyImage
    , CopyImage
    -- * Request Lenses
    , ciClientToken
    , ciEncrypted
    , ciKMSKeyId
    , ciDescription
    , ciDryRun
    , ciName
    , ciSourceImageId
    , ciSourceRegion

    -- * Destructuring the Response
    , copyImageResponse
    , CopyImageResponse
    -- * Response Lenses
    , coprsImageId
    , coprsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
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
  { _ciClientToken   :: !(Maybe Text)
  , _ciEncrypted     :: !(Maybe Bool)
  , _ciKMSKeyId      :: !(Maybe Text)
  , _ciDescription   :: !(Maybe Text)
  , _ciDryRun        :: !(Maybe Bool)
  , _ciName          :: !Text
  , _ciSourceImageId :: !Text
  , _ciSourceRegion  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CopyImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciClientToken' - Unique, case-sensitive identifier you provide to ensure idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'ciEncrypted' - Specifies whether the destination snapshots of the copied image should be encrypted. The default CMK for EBS is used unless a non-default AWS Key Management Service (AWS KMS) CMK is specified with @KmsKeyId@ . For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'ciKMSKeyId' - An identifier for the AWS Key Management Service (AWS KMS) customer master key (CMK) to use when creating the encrypted volume. This parameter is only required if you want to use a non-default CMK; if this parameter is not specified, the default CMK for EBS is used. If a @KmsKeyId@ is specified, the @Encrypted@ flag must also be set.  The CMK identifier may be provided in any of the following formats:      * Key ID     * Key alias, in the form @alias//ExampleAlias/ @      * ARN using key ID. The ID ARN contains the @arn:aws:kms@ namespace, followed by the region of the CMK, the AWS account ID of the CMK owner, the @key@ namespace, and then the CMK ID. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :key//abcd1234-a123-456a-a12b-a123b4cd56ef/ .      * ARN using key alias. The alias ARN contains the @arn:aws:kms@ namespace, followed by the region of the CMK, the AWS account ID of the CMK owner, the @alias@ namespace, and then the CMK alias. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :alias//ExampleAlias/ .  AWS parses @KmsKeyId@ asynchronously, meaning that the action you call may appear to complete even though you provided an invalid identifier. This action will eventually report failure.  The specified CMK must exist in the region that the snapshot is being copied to.
--
-- * 'ciDescription' - A description for the new AMI in the destination region.
--
-- * 'ciDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'ciName' - The name of the new AMI in the destination region.
--
-- * 'ciSourceImageId' - The ID of the AMI to copy.
--
-- * 'ciSourceRegion' - The name of the region that contains the AMI to copy.
copyImage
    :: Text -- ^ 'ciName'
    -> Text -- ^ 'ciSourceImageId'
    -> Text -- ^ 'ciSourceRegion'
    -> CopyImage
copyImage pName_ pSourceImageId_ pSourceRegion_ =
  CopyImage'
    { _ciClientToken = Nothing
    , _ciEncrypted = Nothing
    , _ciKMSKeyId = Nothing
    , _ciDescription = Nothing
    , _ciDryRun = Nothing
    , _ciName = pName_
    , _ciSourceImageId = pSourceImageId_
    , _ciSourceRegion = pSourceRegion_
    }


-- | Unique, case-sensitive identifier you provide to ensure idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> in the /Amazon Elastic Compute Cloud User Guide/ .
ciClientToken :: Lens' CopyImage (Maybe Text)
ciClientToken = lens _ciClientToken (\ s a -> s{_ciClientToken = a})

-- | Specifies whether the destination snapshots of the copied image should be encrypted. The default CMK for EBS is used unless a non-default AWS Key Management Service (AWS KMS) CMK is specified with @KmsKeyId@ . For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
ciEncrypted :: Lens' CopyImage (Maybe Bool)
ciEncrypted = lens _ciEncrypted (\ s a -> s{_ciEncrypted = a})

-- | An identifier for the AWS Key Management Service (AWS KMS) customer master key (CMK) to use when creating the encrypted volume. This parameter is only required if you want to use a non-default CMK; if this parameter is not specified, the default CMK for EBS is used. If a @KmsKeyId@ is specified, the @Encrypted@ flag must also be set.  The CMK identifier may be provided in any of the following formats:      * Key ID     * Key alias, in the form @alias//ExampleAlias/ @      * ARN using key ID. The ID ARN contains the @arn:aws:kms@ namespace, followed by the region of the CMK, the AWS account ID of the CMK owner, the @key@ namespace, and then the CMK ID. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :key//abcd1234-a123-456a-a12b-a123b4cd56ef/ .      * ARN using key alias. The alias ARN contains the @arn:aws:kms@ namespace, followed by the region of the CMK, the AWS account ID of the CMK owner, the @alias@ namespace, and then the CMK alias. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :alias//ExampleAlias/ .  AWS parses @KmsKeyId@ asynchronously, meaning that the action you call may appear to complete even though you provided an invalid identifier. This action will eventually report failure.  The specified CMK must exist in the region that the snapshot is being copied to.
ciKMSKeyId :: Lens' CopyImage (Maybe Text)
ciKMSKeyId = lens _ciKMSKeyId (\ s a -> s{_ciKMSKeyId = a})

-- | A description for the new AMI in the destination region.
ciDescription :: Lens' CopyImage (Maybe Text)
ciDescription = lens _ciDescription (\ s a -> s{_ciDescription = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
ciDryRun :: Lens' CopyImage (Maybe Bool)
ciDryRun = lens _ciDryRun (\ s a -> s{_ciDryRun = a})

-- | The name of the new AMI in the destination region.
ciName :: Lens' CopyImage Text
ciName = lens _ciName (\ s a -> s{_ciName = a})

-- | The ID of the AMI to copy.
ciSourceImageId :: Lens' CopyImage Text
ciSourceImageId = lens _ciSourceImageId (\ s a -> s{_ciSourceImageId = a})

-- | The name of the region that contains the AMI to copy.
ciSourceRegion :: Lens' CopyImage Text
ciSourceRegion = lens _ciSourceRegion (\ s a -> s{_ciSourceRegion = a})

instance AWSRequest CopyImage where
        type Rs CopyImage = CopyImageResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CopyImageResponse' <$>
                   (x .@? "imageId") <*> (pure (fromEnum s)))

instance Hashable CopyImage where

instance NFData CopyImage where

instance ToHeaders CopyImage where
        toHeaders = const mempty

instance ToPath CopyImage where
        toPath = const "/"

instance ToQuery CopyImage where
        toQuery CopyImage'{..}
          = mconcat
              ["Action" =: ("CopyImage" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "ClientToken" =: _ciClientToken,
               "Encrypted" =: _ciEncrypted,
               "KmsKeyId" =: _ciKMSKeyId,
               "Description" =: _ciDescription,
               "DryRun" =: _ciDryRun, "Name" =: _ciName,
               "SourceImageId" =: _ciSourceImageId,
               "SourceRegion" =: _ciSourceRegion]

-- | Contains the output of CopyImage.
--
--
--
-- /See:/ 'copyImageResponse' smart constructor.
data CopyImageResponse = CopyImageResponse'
  { _coprsImageId        :: !(Maybe Text)
  , _coprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CopyImageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coprsImageId' - The ID of the new AMI.
--
-- * 'coprsResponseStatus' - -- | The response status code.
copyImageResponse
    :: Int -- ^ 'coprsResponseStatus'
    -> CopyImageResponse
copyImageResponse pResponseStatus_ =
  CopyImageResponse'
    {_coprsImageId = Nothing, _coprsResponseStatus = pResponseStatus_}


-- | The ID of the new AMI.
coprsImageId :: Lens' CopyImageResponse (Maybe Text)
coprsImageId = lens _coprsImageId (\ s a -> s{_coprsImageId = a})

-- | -- | The response status code.
coprsResponseStatus :: Lens' CopyImageResponse Int
coprsResponseStatus = lens _coprsResponseStatus (\ s a -> s{_coprsResponseStatus = a})

instance NFData CopyImageResponse where
