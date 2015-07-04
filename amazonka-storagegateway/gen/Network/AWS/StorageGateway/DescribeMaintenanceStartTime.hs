{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.StorageGateway.DescribeMaintenanceStartTime
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

-- | This operation returns your gateway\'s weekly maintenance start time
-- including the day and time of the week. Note that values are in terms of
-- the gateway\'s time zone.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DescribeMaintenanceStartTime.html>
module Network.AWS.StorageGateway.DescribeMaintenanceStartTime
    (
    -- * Request
      DescribeMaintenanceStartTime
    -- ** Request constructor
    , describeMaintenanceStartTime
    -- ** Request lenses
    , dmstGatewayARN

    -- * Response
    , DescribeMaintenanceStartTimeResponse
    -- ** Response constructor
    , describeMaintenanceStartTimeResponse
    -- ** Response lenses
    , dmstrGatewayARN
    , dmstrMinuteOfHour
    , dmstrHourOfDay
    , dmstrTimezone
    , dmstrDayOfWeek
    , dmstrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | A JSON object containing the of the gateway.
--
-- /See:/ 'describeMaintenanceStartTime' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmstGatewayARN'
newtype DescribeMaintenanceStartTime = DescribeMaintenanceStartTime'
    { _dmstGatewayARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeMaintenanceStartTime' smart constructor.
describeMaintenanceStartTime :: Text -> DescribeMaintenanceStartTime
describeMaintenanceStartTime pGatewayARN =
    DescribeMaintenanceStartTime'
    { _dmstGatewayARN = pGatewayARN
    }

-- | FIXME: Undocumented member.
dmstGatewayARN :: Lens' DescribeMaintenanceStartTime Text
dmstGatewayARN = lens _dmstGatewayARN (\ s a -> s{_dmstGatewayARN = a});

instance AWSRequest DescribeMaintenanceStartTime
         where
        type Sv DescribeMaintenanceStartTime = StorageGateway
        type Rs DescribeMaintenanceStartTime =
             DescribeMaintenanceStartTimeResponse
        request = postJSON
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
          = object ["GatewayARN" .= _dmstGatewayARN]

instance ToPath DescribeMaintenanceStartTime where
        toPath = const "/"

instance ToQuery DescribeMaintenanceStartTime where
        toQuery = const mempty

-- | /See:/ 'describeMaintenanceStartTimeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmstrGatewayARN'
--
-- * 'dmstrMinuteOfHour'
--
-- * 'dmstrHourOfDay'
--
-- * 'dmstrTimezone'
--
-- * 'dmstrDayOfWeek'
--
-- * 'dmstrStatus'
data DescribeMaintenanceStartTimeResponse = DescribeMaintenanceStartTimeResponse'
    { _dmstrGatewayARN   :: !(Maybe Text)
    , _dmstrMinuteOfHour :: !(Maybe Nat)
    , _dmstrHourOfDay    :: !(Maybe Nat)
    , _dmstrTimezone     :: !(Maybe Text)
    , _dmstrDayOfWeek    :: !(Maybe Nat)
    , _dmstrStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeMaintenanceStartTimeResponse' smart constructor.
describeMaintenanceStartTimeResponse :: Int -> DescribeMaintenanceStartTimeResponse
describeMaintenanceStartTimeResponse pStatus =
    DescribeMaintenanceStartTimeResponse'
    { _dmstrGatewayARN = Nothing
    , _dmstrMinuteOfHour = Nothing
    , _dmstrHourOfDay = Nothing
    , _dmstrTimezone = Nothing
    , _dmstrDayOfWeek = Nothing
    , _dmstrStatus = pStatus
    }

-- | FIXME: Undocumented member.
dmstrGatewayARN :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Text)
dmstrGatewayARN = lens _dmstrGatewayARN (\ s a -> s{_dmstrGatewayARN = a});

-- | FIXME: Undocumented member.
dmstrMinuteOfHour :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Natural)
dmstrMinuteOfHour = lens _dmstrMinuteOfHour (\ s a -> s{_dmstrMinuteOfHour = a}) . mapping _Nat;

-- | FIXME: Undocumented member.
dmstrHourOfDay :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Natural)
dmstrHourOfDay = lens _dmstrHourOfDay (\ s a -> s{_dmstrHourOfDay = a}) . mapping _Nat;

-- | FIXME: Undocumented member.
dmstrTimezone :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Text)
dmstrTimezone = lens _dmstrTimezone (\ s a -> s{_dmstrTimezone = a});

-- | FIXME: Undocumented member.
dmstrDayOfWeek :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Natural)
dmstrDayOfWeek = lens _dmstrDayOfWeek (\ s a -> s{_dmstrDayOfWeek = a}) . mapping _Nat;

-- | FIXME: Undocumented member.
dmstrStatus :: Lens' DescribeMaintenanceStartTimeResponse Int
dmstrStatus = lens _dmstrStatus (\ s a -> s{_dmstrStatus = a});
