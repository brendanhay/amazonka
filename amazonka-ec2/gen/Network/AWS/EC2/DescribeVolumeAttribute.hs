{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.DescribeVolumeAttribute
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes the specified attribute of the specified volume. You can
-- specify only one attribute at a time.
--
-- For more information about EBS volumes, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumes.html Amazon EBS Volumes>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVolumeAttribute.html>
module Network.AWS.EC2.DescribeVolumeAttribute
    (
    -- * Request
      DescribeVolumeAttribute
    -- ** Request constructor
    , describeVolumeAttribute
    -- ** Request lenses
    , dvaAttribute
    , dvaDryRun
    , dvaVolumeId

    -- * Response
    , DescribeVolumeAttributeResponse
    -- ** Response constructor
    , describeVolumeAttributeResponse
    -- ** Response lenses
    , dvarProductCodes
    , dvarVolumeId
    , dvarAutoEnableIO
    , dvarStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeVolumeAttribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvaAttribute'
--
-- * 'dvaDryRun'
--
-- * 'dvaVolumeId'
data DescribeVolumeAttribute = DescribeVolumeAttribute'
    { _dvaAttribute :: !(Maybe VolumeAttributeName)
    , _dvaDryRun    :: !(Maybe Bool)
    , _dvaVolumeId  :: !Text
    } deriving (Eq,Read,Show)

-- | 'DescribeVolumeAttribute' smart constructor.
describeVolumeAttribute :: Text -> DescribeVolumeAttribute
describeVolumeAttribute pVolumeId =
    DescribeVolumeAttribute'
    { _dvaAttribute = Nothing
    , _dvaDryRun = Nothing
    , _dvaVolumeId = pVolumeId
    }

-- | The instance attribute.
dvaAttribute :: Lens' DescribeVolumeAttribute (Maybe VolumeAttributeName)
dvaAttribute = lens _dvaAttribute (\ s a -> s{_dvaAttribute = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dvaDryRun :: Lens' DescribeVolumeAttribute (Maybe Bool)
dvaDryRun = lens _dvaDryRun (\ s a -> s{_dvaDryRun = a});

-- | The ID of the volume.
dvaVolumeId :: Lens' DescribeVolumeAttribute Text
dvaVolumeId = lens _dvaVolumeId (\ s a -> s{_dvaVolumeId = a});

instance AWSRequest DescribeVolumeAttribute where
        type Sv DescribeVolumeAttribute = EC2
        type Rs DescribeVolumeAttribute =
             DescribeVolumeAttributeResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeVolumeAttributeResponse' <$>
                   (x .@? "productCodes" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "volumeId")
                     <*> (x .@? "autoEnableIO")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeVolumeAttribute where
        toHeaders = const mempty

instance ToPath DescribeVolumeAttribute where
        toPath = const "/"

instance ToQuery DescribeVolumeAttribute where
        toQuery DescribeVolumeAttribute'{..}
          = mconcat
              ["Action" =:
                 ("DescribeVolumeAttribute" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "Attribute" =: _dvaAttribute, "DryRun" =: _dvaDryRun,
               "VolumeId" =: _dvaVolumeId]

-- | /See:/ 'describeVolumeAttributeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvarProductCodes'
--
-- * 'dvarVolumeId'
--
-- * 'dvarAutoEnableIO'
--
-- * 'dvarStatus'
data DescribeVolumeAttributeResponse = DescribeVolumeAttributeResponse'
    { _dvarProductCodes :: !(Maybe [ProductCode])
    , _dvarVolumeId     :: !(Maybe Text)
    , _dvarAutoEnableIO :: !(Maybe AttributeBooleanValue)
    , _dvarStatus       :: !Int
    } deriving (Eq,Read,Show)

-- | 'DescribeVolumeAttributeResponse' smart constructor.
describeVolumeAttributeResponse :: Int -> DescribeVolumeAttributeResponse
describeVolumeAttributeResponse pStatus =
    DescribeVolumeAttributeResponse'
    { _dvarProductCodes = Nothing
    , _dvarVolumeId = Nothing
    , _dvarAutoEnableIO = Nothing
    , _dvarStatus = pStatus
    }

-- | A list of product codes.
dvarProductCodes :: Lens' DescribeVolumeAttributeResponse [ProductCode]
dvarProductCodes = lens _dvarProductCodes (\ s a -> s{_dvarProductCodes = a}) . _Default;

-- | The ID of the volume.
dvarVolumeId :: Lens' DescribeVolumeAttributeResponse (Maybe Text)
dvarVolumeId = lens _dvarVolumeId (\ s a -> s{_dvarVolumeId = a});

-- | The state of @autoEnableIO@ attribute.
dvarAutoEnableIO :: Lens' DescribeVolumeAttributeResponse (Maybe AttributeBooleanValue)
dvarAutoEnableIO = lens _dvarAutoEnableIO (\ s a -> s{_dvarAutoEnableIO = a});

-- | FIXME: Undocumented member.
dvarStatus :: Lens' DescribeVolumeAttributeResponse Int
dvarStatus = lens _dvarStatus (\ s a -> s{_dvarStatus = a});
