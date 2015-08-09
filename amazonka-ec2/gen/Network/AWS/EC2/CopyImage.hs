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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CopyImage.html AWS API Reference> for CopyImage.
module Network.AWS.EC2.CopyImage
    (
    -- * Creating a Request
      copyImage
    , CopyImage
    -- * Request Lenses
    , ciClientToken
    , ciDryRun
    , ciDescription
    , ciSourceRegion
    , ciSourceImageId
    , ciName

    -- * Destructuring the Response
    , copyImageResponse
    , CopyImageResponse
    -- * Response Lenses
    , coprsImageId
    , coprsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'copyImage' smart constructor.
data CopyImage = CopyImage'
    { _ciClientToken   :: !(Maybe Text)
    , _ciDryRun        :: !(Maybe Bool)
    , _ciDescription   :: !(Maybe Text)
    , _ciSourceRegion  :: !Text
    , _ciSourceImageId :: !Text
    , _ciName          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CopyImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
copyImage
    :: Text -- ^ 'ciSourceRegion'
    -> Text -- ^ 'ciSourceImageId'
    -> Text -- ^ 'ciName'
    -> CopyImage
copyImage pSourceRegion_ pSourceImageId_ pName_ =
    CopyImage'
    { _ciClientToken = Nothing
    , _ciDryRun = Nothing
    , _ciDescription = Nothing
    , _ciSourceRegion = pSourceRegion_
    , _ciSourceImageId = pSourceImageId_
    , _ciName = pName_
    }

-- | Unique, case-sensitive identifier you provide to ensure idempotency of
-- the request. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>
-- in the /Amazon Elastic Compute Cloud User Guide/.
ciClientToken :: Lens' CopyImage (Maybe Text)
ciClientToken = lens _ciClientToken (\ s a -> s{_ciClientToken = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
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
               "ClientToken" =: _ciClientToken,
               "DryRun" =: _ciDryRun,
               "Description" =: _ciDescription,
               "SourceRegion" =: _ciSourceRegion,
               "SourceImageId" =: _ciSourceImageId,
               "Name" =: _ciName]

-- | /See:/ 'copyImageResponse' smart constructor.
data CopyImageResponse = CopyImageResponse'
    { _coprsImageId :: !(Maybe Text)
    , _coprsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CopyImageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coprsImageId'
--
-- * 'coprsStatus'
copyImageResponse
    :: Int -- ^ 'coprsStatus'
    -> CopyImageResponse
copyImageResponse pStatus_ =
    CopyImageResponse'
    { _coprsImageId = Nothing
    , _coprsStatus = pStatus_
    }

-- | The ID of the new AMI.
coprsImageId :: Lens' CopyImageResponse (Maybe Text)
coprsImageId = lens _coprsImageId (\ s a -> s{_coprsImageId = a});

-- | The response status code.
coprsStatus :: Lens' CopyImageResponse Int
coprsStatus = lens _coprsStatus (\ s a -> s{_coprsStatus = a});
