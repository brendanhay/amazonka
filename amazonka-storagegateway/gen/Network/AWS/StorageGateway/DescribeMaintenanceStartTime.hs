{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeMaintenanceStartTime
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation returns your gateway\'s weekly maintenance start time
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
    , dmstrqGatewayARN

    -- * Response
    , DescribeMaintenanceStartTimeResponse
    -- ** Response constructor
    , describeMaintenanceStartTimeResponse
    -- ** Response lenses
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

-- | A JSON object containing the of the gateway.
--
-- /See:/ 'describeMaintenanceStartTime' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmstrqGatewayARN'
newtype DescribeMaintenanceStartTime = DescribeMaintenanceStartTime'
    { _dmstrqGatewayARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeMaintenanceStartTime' smart constructor.
describeMaintenanceStartTime :: Text -> DescribeMaintenanceStartTime
describeMaintenanceStartTime pGatewayARN_ =
    DescribeMaintenanceStartTime'
    { _dmstrqGatewayARN = pGatewayARN_
    }

-- | FIXME: Undocumented member.
dmstrqGatewayARN :: Lens' DescribeMaintenanceStartTime Text
dmstrqGatewayARN = lens _dmstrqGatewayARN (\ s a -> s{_dmstrqGatewayARN = a});

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
          = object ["GatewayARN" .= _dmstrqGatewayARN]

instance ToPath DescribeMaintenanceStartTime where
        toPath = const "/"

instance ToQuery DescribeMaintenanceStartTime where
        toQuery = const mempty

-- | /See:/ 'describeMaintenanceStartTimeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
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
data DescribeMaintenanceStartTimeResponse = DescribeMaintenanceStartTimeResponse'
    { _dmstrsGatewayARN   :: !(Maybe Text)
    , _dmstrsMinuteOfHour :: !(Maybe Nat)
    , _dmstrsHourOfDay    :: !(Maybe Nat)
    , _dmstrsTimezone     :: !(Maybe Text)
    , _dmstrsDayOfWeek    :: !(Maybe Nat)
    , _dmstrsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeMaintenanceStartTimeResponse' smart constructor.
describeMaintenanceStartTimeResponse :: Int -> DescribeMaintenanceStartTimeResponse
describeMaintenanceStartTimeResponse pStatus_ =
    DescribeMaintenanceStartTimeResponse'
    { _dmstrsGatewayARN = Nothing
    , _dmstrsMinuteOfHour = Nothing
    , _dmstrsHourOfDay = Nothing
    , _dmstrsTimezone = Nothing
    , _dmstrsDayOfWeek = Nothing
    , _dmstrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
dmstrsGatewayARN :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Text)
dmstrsGatewayARN = lens _dmstrsGatewayARN (\ s a -> s{_dmstrsGatewayARN = a});

-- | FIXME: Undocumented member.
dmstrsMinuteOfHour :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Natural)
dmstrsMinuteOfHour = lens _dmstrsMinuteOfHour (\ s a -> s{_dmstrsMinuteOfHour = a}) . mapping _Nat;

-- | FIXME: Undocumented member.
dmstrsHourOfDay :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Natural)
dmstrsHourOfDay = lens _dmstrsHourOfDay (\ s a -> s{_dmstrsHourOfDay = a}) . mapping _Nat;

-- | FIXME: Undocumented member.
dmstrsTimezone :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Text)
dmstrsTimezone = lens _dmstrsTimezone (\ s a -> s{_dmstrsTimezone = a});

-- | FIXME: Undocumented member.
dmstrsDayOfWeek :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Natural)
dmstrsDayOfWeek = lens _dmstrsDayOfWeek (\ s a -> s{_dmstrsDayOfWeek = a}) . mapping _Nat;

-- | FIXME: Undocumented member.
dmstrsStatus :: Lens' DescribeMaintenanceStartTimeResponse Int
dmstrsStatus = lens _dmstrsStatus (\ s a -> s{_dmstrsStatus = a});
