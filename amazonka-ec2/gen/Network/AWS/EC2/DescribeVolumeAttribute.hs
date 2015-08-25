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
-- Module      : Network.AWS.EC2.DescribeVolumeAttribute
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified attribute of the specified volume. You can
-- specify only one attribute at a time.
--
-- For more information about EBS volumes, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumes.html Amazon EBS Volumes>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVolumeAttribute.html AWS API Reference> for DescribeVolumeAttribute.
module Network.AWS.EC2.DescribeVolumeAttribute
    (
    -- * Creating a Request
      describeVolumeAttribute
    , DescribeVolumeAttribute
    -- * Request Lenses
    , dvaAttribute
    , dvaDryRun
    , dvaVolumeId

    -- * Destructuring the Response
    , describeVolumeAttributeResponse
    , DescribeVolumeAttributeResponse
    -- * Response Lenses
    , dvarsProductCodes
    , dvarsVolumeId
    , dvarsAutoEnableIO
    , dvarsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeVolumeAttribute' smart constructor.
data DescribeVolumeAttribute = DescribeVolumeAttribute'
    { _dvaAttribute :: !(Maybe VolumeAttributeName)
    , _dvaDryRun    :: !(Maybe Bool)
    , _dvaVolumeId  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeVolumeAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvaAttribute'
--
-- * 'dvaDryRun'
--
-- * 'dvaVolumeId'
describeVolumeAttribute
    :: Text -- ^ 'dvaVolumeId'
    -> DescribeVolumeAttribute
describeVolumeAttribute pVolumeId_ =
    DescribeVolumeAttribute'
    { _dvaAttribute = Nothing
    , _dvaDryRun = Nothing
    , _dvaVolumeId = pVolumeId_
    }

-- | The instance attribute.
dvaAttribute :: Lens' DescribeVolumeAttribute (Maybe VolumeAttributeName)
dvaAttribute = lens _dvaAttribute (\ s a -> s{_dvaAttribute = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
dvaDryRun :: Lens' DescribeVolumeAttribute (Maybe Bool)
dvaDryRun = lens _dvaDryRun (\ s a -> s{_dvaDryRun = a});

-- | The ID of the volume.
dvaVolumeId :: Lens' DescribeVolumeAttribute Text
dvaVolumeId = lens _dvaVolumeId (\ s a -> s{_dvaVolumeId = a});

instance AWSRequest DescribeVolumeAttribute where
        type Rs DescribeVolumeAttribute =
             DescribeVolumeAttributeResponse
        request = postQuery eC2
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
data DescribeVolumeAttributeResponse = DescribeVolumeAttributeResponse'
    { _dvarsProductCodes :: !(Maybe [ProductCode])
    , _dvarsVolumeId     :: !(Maybe Text)
    , _dvarsAutoEnableIO :: !(Maybe AttributeBooleanValue)
    , _dvarsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeVolumeAttributeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvarsProductCodes'
--
-- * 'dvarsVolumeId'
--
-- * 'dvarsAutoEnableIO'
--
-- * 'dvarsStatus'
describeVolumeAttributeResponse
    :: Int -- ^ 'dvarsStatus'
    -> DescribeVolumeAttributeResponse
describeVolumeAttributeResponse pStatus_ =
    DescribeVolumeAttributeResponse'
    { _dvarsProductCodes = Nothing
    , _dvarsVolumeId = Nothing
    , _dvarsAutoEnableIO = Nothing
    , _dvarsStatus = pStatus_
    }

-- | A list of product codes.
dvarsProductCodes :: Lens' DescribeVolumeAttributeResponse [ProductCode]
dvarsProductCodes = lens _dvarsProductCodes (\ s a -> s{_dvarsProductCodes = a}) . _Default . _Coerce;

-- | The ID of the volume.
dvarsVolumeId :: Lens' DescribeVolumeAttributeResponse (Maybe Text)
dvarsVolumeId = lens _dvarsVolumeId (\ s a -> s{_dvarsVolumeId = a});

-- | The state of 'autoEnableIO' attribute.
dvarsAutoEnableIO :: Lens' DescribeVolumeAttributeResponse (Maybe AttributeBooleanValue)
dvarsAutoEnableIO = lens _dvarsAutoEnableIO (\ s a -> s{_dvarsAutoEnableIO = a});

-- | The response status code.
dvarsStatus :: Lens' DescribeVolumeAttributeResponse Int
dvarsStatus = lens _dvarsStatus (\ s a -> s{_dvarsStatus = a});
