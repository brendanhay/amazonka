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
-- Module      : Network.AWS.StorageGateway.DescribeMaintenanceStartTime
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns your gateway\'s weekly maintenance start time
-- including the day and time of the week. Note that values are in terms of
-- the gateway\'s time zone.
--
-- /See:/ <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DescribeMaintenanceStartTime.html AWS API Reference> for DescribeMaintenanceStartTime.
module Network.AWS.StorageGateway.DescribeMaintenanceStartTime
    (
    -- * Creating a Request
      describeMaintenanceStartTime
    , DescribeMaintenanceStartTime
    -- * Request Lenses
    , dmstGatewayARN

    -- * Destructuring the Response
    , describeMaintenanceStartTimeResponse
    , DescribeMaintenanceStartTimeResponse
    -- * Response Lenses
    , dmstrsGatewayARN
    , dmstrsMinuteOfHour
    , dmstrsHourOfDay
    , dmstrsTimezone
    , dmstrsDayOfWeek
    , dmstrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types
import           Network.AWS.StorageGateway.Types.Product

-- | A JSON object containing the of the gateway.
--
-- /See:/ 'describeMaintenanceStartTime' smart constructor.
newtype DescribeMaintenanceStartTime = DescribeMaintenanceStartTime'
    { _dmstGatewayARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeMaintenanceStartTime' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmstGatewayARN'
describeMaintenanceStartTime
    :: Text -- ^ 'dmstGatewayARN'
    -> DescribeMaintenanceStartTime
describeMaintenanceStartTime pGatewayARN_ =
    DescribeMaintenanceStartTime'
    { _dmstGatewayARN = pGatewayARN_
    }

-- | Undocumented member.
dmstGatewayARN :: Lens' DescribeMaintenanceStartTime Text
dmstGatewayARN = lens _dmstGatewayARN (\ s a -> s{_dmstGatewayARN = a});

instance AWSRequest DescribeMaintenanceStartTime
         where
        type Rs DescribeMaintenanceStartTime =
             DescribeMaintenanceStartTimeResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 DescribeMaintenanceStartTimeResponse' <$>
                   (x .?> "GatewayARN") <*> (x .?> "MinuteOfHour") <*>
                     (x .?> "HourOfDay")
                     <*> (x .?> "Timezone")
                     <*> (x .?> "DayOfWeek")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeMaintenanceStartTime where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.DescribeMaintenanceStartTime"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeMaintenanceStartTime where
        toJSON DescribeMaintenanceStartTime'{..}
          = object
              (catMaybes [Just ("GatewayARN" .= _dmstGatewayARN)])

instance ToPath DescribeMaintenanceStartTime where
        toPath = const "/"

instance ToQuery DescribeMaintenanceStartTime where
        toQuery = const mempty

-- | /See:/ 'describeMaintenanceStartTimeResponse' smart constructor.
data DescribeMaintenanceStartTimeResponse = DescribeMaintenanceStartTimeResponse'
    { _dmstrsGatewayARN   :: !(Maybe Text)
    , _dmstrsMinuteOfHour :: !(Maybe Nat)
    , _dmstrsHourOfDay    :: !(Maybe Nat)
    , _dmstrsTimezone     :: !(Maybe Text)
    , _dmstrsDayOfWeek    :: !(Maybe Nat)
    , _dmstrsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeMaintenanceStartTimeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmstrsGatewayARN'
--
-- * 'dmstrsMinuteOfHour'
--
-- * 'dmstrsHourOfDay'
--
-- * 'dmstrsTimezone'
--
-- * 'dmstrsDayOfWeek'
--
-- * 'dmstrsStatus'
describeMaintenanceStartTimeResponse
    :: Int -- ^ 'dmstrsStatus'
    -> DescribeMaintenanceStartTimeResponse
describeMaintenanceStartTimeResponse pStatus_ =
    DescribeMaintenanceStartTimeResponse'
    { _dmstrsGatewayARN = Nothing
    , _dmstrsMinuteOfHour = Nothing
    , _dmstrsHourOfDay = Nothing
    , _dmstrsTimezone = Nothing
    , _dmstrsDayOfWeek = Nothing
    , _dmstrsStatus = pStatus_
    }

-- | Undocumented member.
dmstrsGatewayARN :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Text)
dmstrsGatewayARN = lens _dmstrsGatewayARN (\ s a -> s{_dmstrsGatewayARN = a});

-- | Undocumented member.
dmstrsMinuteOfHour :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Natural)
dmstrsMinuteOfHour = lens _dmstrsMinuteOfHour (\ s a -> s{_dmstrsMinuteOfHour = a}) . mapping _Nat;

-- | Undocumented member.
dmstrsHourOfDay :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Natural)
dmstrsHourOfDay = lens _dmstrsHourOfDay (\ s a -> s{_dmstrsHourOfDay = a}) . mapping _Nat;

-- | Undocumented member.
dmstrsTimezone :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Text)
dmstrsTimezone = lens _dmstrsTimezone (\ s a -> s{_dmstrsTimezone = a});

-- | Undocumented member.
dmstrsDayOfWeek :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Natural)
dmstrsDayOfWeek = lens _dmstrsDayOfWeek (\ s a -> s{_dmstrsDayOfWeek = a}) . mapping _Nat;

-- | The response status code.
dmstrsStatus :: Lens' DescribeMaintenanceStartTimeResponse Int
dmstrsStatus = lens _dmstrsStatus (\ s a -> s{_dmstrsStatus = a});
