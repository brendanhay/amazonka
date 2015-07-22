{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CopyImage
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Initiates the copy of an AMI from the specified source region to the
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
    , cirqClientToken
    , cirqDryRun
    , cirqDescription
    , cirqSourceRegion
    , cirqSourceImageId
    , cirqName

    -- * Response
    , CopyImageResponse
    -- ** Response constructor
    , copyImageResponse
    -- ** Response lenses
    , coprsImageId
    , coprsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'copyImage' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cirqClientToken'
--
-- * 'cirqDryRun'
--
-- * 'cirqDescription'
--
-- * 'cirqSourceRegion'
--
-- * 'cirqSourceImageId'
--
-- * 'cirqName'
data CopyImage = CopyImage'
    { _cirqClientToken   :: !(Maybe Text)
    , _cirqDryRun        :: !(Maybe Bool)
    , _cirqDescription   :: !(Maybe Text)
    , _cirqSourceRegion  :: !Text
    , _cirqSourceImageId :: !Text
    , _cirqName          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CopyImage' smart constructor.
copyImage :: Text -> Text -> Text -> CopyImage
copyImage pSourceRegion pSourceImageId pName =
    CopyImage'
    { _cirqClientToken = Nothing
    , _cirqDryRun = Nothing
    , _cirqDescription = Nothing
    , _cirqSourceRegion = pSourceRegion
    , _cirqSourceImageId = pSourceImageId
    , _cirqName = pName
    }

-- | Unique, case-sensitive identifier you provide to ensure idempotency of
-- the request. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>
-- in the /Amazon Elastic Compute Cloud User Guide/.
cirqClientToken :: Lens' CopyImage (Maybe Text)
cirqClientToken = lens _cirqClientToken (\ s a -> s{_cirqClientToken = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cirqDryRun :: Lens' CopyImage (Maybe Bool)
cirqDryRun = lens _cirqDryRun (\ s a -> s{_cirqDryRun = a});

-- | A description for the new AMI in the destination region.
cirqDescription :: Lens' CopyImage (Maybe Text)
cirqDescription = lens _cirqDescription (\ s a -> s{_cirqDescription = a});

-- | The name of the region that contains the AMI to copy.
cirqSourceRegion :: Lens' CopyImage Text
cirqSourceRegion = lens _cirqSourceRegion (\ s a -> s{_cirqSourceRegion = a});

-- | The ID of the AMI to copy.
cirqSourceImageId :: Lens' CopyImage Text
cirqSourceImageId = lens _cirqSourceImageId (\ s a -> s{_cirqSourceImageId = a});

-- | The name of the new AMI in the destination region.
cirqName :: Lens' CopyImage Text
cirqName = lens _cirqName (\ s a -> s{_cirqName = a});

instance AWSRequest CopyImage where
        type Sv CopyImage = EC2
        type Rs CopyImage = CopyImageResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 CopyImageResponse' <$>
                   (x .@? "imageId") <*> (pure (fromEnum s)))

instance ToHeaders CopyImage where
        toHeaders = const mempty

instance ToPath CopyImage where
        toPath = const "/"

instance ToQuery CopyImage where
        toQuery CopyImage'{..}
          = mconcat
              ["Action" =: ("CopyImage" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "ClientToken" =: _cirqClientToken,
               "DryRun" =: _cirqDryRun,
               "Description" =: _cirqDescription,
               "SourceRegion" =: _cirqSourceRegion,
               "SourceImageId" =: _cirqSourceImageId,
               "Name" =: _cirqName]

-- | /See:/ 'copyImageResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'coprsImageId'
--
-- * 'coprsStatus'
data CopyImageResponse = CopyImageResponse'
    { _coprsImageId :: !(Maybe Text)
    , _coprsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CopyImageResponse' smart constructor.
copyImageResponse :: Int -> CopyImageResponse
copyImageResponse pStatus =
    CopyImageResponse'
    { _coprsImageId = Nothing
    , _coprsStatus = pStatus
    }

-- | The ID of the new AMI.
coprsImageId :: Lens' CopyImageResponse (Maybe Text)
coprsImageId = lens _coprsImageId (\ s a -> s{_coprsImageId = a});

-- | FIXME: Undocumented member.
coprsStatus :: Lens' CopyImageResponse Int
coprsStatus = lens _coprsStatus (\ s a -> s{_coprsStatus = a});
