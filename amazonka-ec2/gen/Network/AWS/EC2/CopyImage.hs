{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.CopyImage
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Initiates the copy of an AMI from the specified source region to the
-- current region. You specify the destination region by using its endpoint
-- when making the request. AMIs that use encrypted EBS snapshots cannot be
-- copied with this method.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/CopyingAMIs.html Copying AMIs>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CopyImage.html>
module Network.AWS.EC2.CopyImage
    (
    -- * Request
      CopyImage
    -- ** Request constructor
    , copyImage
    -- ** Request lenses
    , ciClientToken
    , ciDryRun
    , ciDescription
    , ciSourceRegion
    , ciSourceImageId
    , ciName

    -- * Response
    , CopyImageResponse
    -- ** Response constructor
    , copyImageResponse
    -- ** Response lenses
    , copImageId
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.EC2.Types

-- | /See:/ 'copyImage' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ciClientToken'
--
-- * 'ciDryRun'
--
-- * 'ciDescription'
--
-- * 'ciSourceRegion'
--
-- * 'ciSourceImageId'
--
-- * 'ciName'
data CopyImage = CopyImage'{_ciClientToken :: Maybe Text, _ciDryRun :: Maybe Bool, _ciDescription :: Maybe Text, _ciSourceRegion :: Text, _ciSourceImageId :: Text, _ciName :: Text} deriving (Eq, Read, Show)

-- | 'CopyImage' smart constructor.
copyImage :: Text -> Text -> Text -> CopyImage
copyImage pSourceRegion pSourceImageId pName = CopyImage'{_ciClientToken = Nothing, _ciDryRun = Nothing, _ciDescription = Nothing, _ciSourceRegion = pSourceRegion, _ciSourceImageId = pSourceImageId, _ciName = pName};

-- | Unique, case-sensitive identifier you provide to ensure idempotency of
-- the request. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>
-- in the /Amazon Elastic Compute Cloud User Guide/.
ciClientToken :: Lens' CopyImage (Maybe Text)
ciClientToken = lens _ciClientToken (\ s a -> s{_ciClientToken = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
ciDryRun :: Lens' CopyImage (Maybe Bool)
ciDryRun = lens _ciDryRun (\ s a -> s{_ciDryRun = a});

-- | A description for the new AMI in the destination region.
ciDescription :: Lens' CopyImage (Maybe Text)
ciDescription = lens _ciDescription (\ s a -> s{_ciDescription = a});

-- | The name of the region that contains the AMI to copy.
ciSourceRegion :: Lens' CopyImage Text
ciSourceRegion = lens _ciSourceRegion (\ s a -> s{_ciSourceRegion = a});

-- | The ID of the AMI to copy.
ciSourceImageId :: Lens' CopyImage Text
ciSourceImageId = lens _ciSourceImageId (\ s a -> s{_ciSourceImageId = a});

-- | The name of the new AMI in the destination region.
ciName :: Lens' CopyImage Text
ciName = lens _ciName (\ s a -> s{_ciName = a});

instance AWSRequest CopyImage where
        type Sv CopyImage = EC2
        type Rs CopyImage = CopyImageResponse
        request = post
        response
          = receiveXML
              (\ s h x -> CopyImageResponse' <$> x .@? "imageId")

instance ToHeaders CopyImage where
        toHeaders = const mempty

instance ToPath CopyImage where
        toPath = const "/"

instance ToQuery CopyImage where
        toQuery CopyImage'{..}
          = mconcat
              ["Action" =: ("CopyImage" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "ClientToken" =: _ciClientToken,
               "DryRun" =: _ciDryRun,
               "Description" =: _ciDescription,
               "SourceRegion" =: _ciSourceRegion,
               "SourceImageId" =: _ciSourceImageId,
               "Name" =: _ciName]

-- | /See:/ 'copyImageResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'copImageId'
newtype CopyImageResponse = CopyImageResponse'{_copImageId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'CopyImageResponse' smart constructor.
copyImageResponse :: CopyImageResponse
copyImageResponse = CopyImageResponse'{_copImageId = Nothing};

-- | The ID of the new AMI.
copImageId :: Lens' CopyImageResponse (Maybe Text)
copImageId = lens _copImageId (\ s a -> s{_copImageId = a});
