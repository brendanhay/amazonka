{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVolumeAttribute
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified attribute of the specified volume. You can
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
    , dvarqAttribute
    , dvarqDryRun
    , dvarqVolumeId

    -- * Response
    , DescribeVolumeAttributeResponse
    -- ** Response constructor
    , describeVolumeAttributeResponse
    -- ** Response lenses
    , dvarsProductCodes
    , dvarsVolumeId
    , dvarsAutoEnableIO
    , dvarsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeVolumeAttribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvarqAttribute'
--
-- * 'dvarqDryRun'
--
-- * 'dvarqVolumeId'
data DescribeVolumeAttribute = DescribeVolumeAttribute'
    { _dvarqAttribute :: !(Maybe VolumeAttributeName)
    , _dvarqDryRun    :: !(Maybe Bool)
    , _dvarqVolumeId  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeVolumeAttribute' smart constructor.
describeVolumeAttribute :: Text -> DescribeVolumeAttribute
describeVolumeAttribute pVolumeId =
    DescribeVolumeAttribute'
    { _dvarqAttribute = Nothing
    , _dvarqDryRun = Nothing
    , _dvarqVolumeId = pVolumeId
    }

-- | The instance attribute.
dvarqAttribute :: Lens' DescribeVolumeAttribute (Maybe VolumeAttributeName)
dvarqAttribute = lens _dvarqAttribute (\ s a -> s{_dvarqAttribute = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dvarqDryRun :: Lens' DescribeVolumeAttribute (Maybe Bool)
dvarqDryRun = lens _dvarqDryRun (\ s a -> s{_dvarqDryRun = a});

-- | The ID of the volume.
dvarqVolumeId :: Lens' DescribeVolumeAttribute Text
dvarqVolumeId = lens _dvarqVolumeId (\ s a -> s{_dvarqVolumeId = a});

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
               "Attribute" =: _dvarqAttribute,
               "DryRun" =: _dvarqDryRun,
               "VolumeId" =: _dvarqVolumeId]

-- | /See:/ 'describeVolumeAttributeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvarsProductCodes'
--
-- * 'dvarsVolumeId'
--
-- * 'dvarsAutoEnableIO'
--
-- * 'dvarsStatus'
data DescribeVolumeAttributeResponse = DescribeVolumeAttributeResponse'
    { _dvarsProductCodes :: !(Maybe [ProductCode])
    , _dvarsVolumeId     :: !(Maybe Text)
    , _dvarsAutoEnableIO :: !(Maybe AttributeBooleanValue)
    , _dvarsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeVolumeAttributeResponse' smart constructor.
describeVolumeAttributeResponse :: Int -> DescribeVolumeAttributeResponse
describeVolumeAttributeResponse pStatus =
    DescribeVolumeAttributeResponse'
    { _dvarsProductCodes = Nothing
    , _dvarsVolumeId = Nothing
    , _dvarsAutoEnableIO = Nothing
    , _dvarsStatus = pStatus
    }

-- | A list of product codes.
dvarsProductCodes :: Lens' DescribeVolumeAttributeResponse [ProductCode]
dvarsProductCodes = lens _dvarsProductCodes (\ s a -> s{_dvarsProductCodes = a}) . _Default;

-- | The ID of the volume.
dvarsVolumeId :: Lens' DescribeVolumeAttributeResponse (Maybe Text)
dvarsVolumeId = lens _dvarsVolumeId (\ s a -> s{_dvarsVolumeId = a});

-- | The state of @autoEnableIO@ attribute.
dvarsAutoEnableIO :: Lens' DescribeVolumeAttributeResponse (Maybe AttributeBooleanValue)
dvarsAutoEnableIO = lens _dvarsAutoEnableIO (\ s a -> s{_dvarsAutoEnableIO = a});

-- | FIXME: Undocumented member.
dvarsStatus :: Lens' DescribeVolumeAttributeResponse Int
dvarsStatus = lens _dvarsStatus (\ s a -> s{_dvarsStatus = a});
