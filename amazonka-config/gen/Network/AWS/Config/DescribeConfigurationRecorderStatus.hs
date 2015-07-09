{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeConfigurationRecorderStatus
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns the current status of the specified configuration recorder. If a
-- configuration recorder is not specified, this action returns the status
-- of all configuration recorder associated with the account.
--
-- Currently, you can specify only one configuration recorder per account.
--
-- <http://docs.aws.amazon.com/config/latest/APIReference/API_DescribeConfigurationRecorderStatus.html>
module Network.AWS.Config.DescribeConfigurationRecorderStatus
    (
    -- * Request
      DescribeConfigurationRecorderStatus
    -- ** Request constructor
    , describeConfigurationRecorderStatus
    -- ** Request lenses
    , dcrsConfigurationRecorderNames

    -- * Response
    , DescribeConfigurationRecorderStatusResponse
    -- ** Response constructor
    , describeConfigurationRecorderStatusResponse
    -- ** Response lenses
    , dcrsrConfigurationRecordersStatus
    , dcrsrStatus
    ) where

import           Network.AWS.Config.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the DescribeConfigurationRecorderStatus action.
--
-- /See:/ 'describeConfigurationRecorderStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcrsConfigurationRecorderNames'
newtype DescribeConfigurationRecorderStatus = DescribeConfigurationRecorderStatus'
    { _dcrsConfigurationRecorderNames :: Maybe [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeConfigurationRecorderStatus' smart constructor.
describeConfigurationRecorderStatus :: DescribeConfigurationRecorderStatus
describeConfigurationRecorderStatus =
    DescribeConfigurationRecorderStatus'
    { _dcrsConfigurationRecorderNames = Nothing
    }

-- | The name(s) of the configuration recorder. If the name is not specified,
-- the action returns the current status of all the configuration recorders
-- associated with the account.
dcrsConfigurationRecorderNames :: Lens' DescribeConfigurationRecorderStatus [Text]
dcrsConfigurationRecorderNames = lens _dcrsConfigurationRecorderNames (\ s a -> s{_dcrsConfigurationRecorderNames = a}) . _Default;

instance AWSRequest
         DescribeConfigurationRecorderStatus where
        type Sv DescribeConfigurationRecorderStatus = Config
        type Rs DescribeConfigurationRecorderStatus =
             DescribeConfigurationRecorderStatusResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeConfigurationRecorderStatusResponse' <$>
                   (x .?> "ConfigurationRecordersStatus" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance ToHeaders
         DescribeConfigurationRecorderStatus where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.DescribeConfigurationRecorderStatus"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeConfigurationRecorderStatus
         where
        toJSON DescribeConfigurationRecorderStatus'{..}
          = object
              ["ConfigurationRecorderNames" .=
                 _dcrsConfigurationRecorderNames]

instance ToPath DescribeConfigurationRecorderStatus
         where
        toPath = const "/"

instance ToQuery DescribeConfigurationRecorderStatus
         where
        toQuery = const mempty

-- | The output for the DescribeConfigurationRecorderStatus action in JSON
-- format.
--
-- /See:/ 'describeConfigurationRecorderStatusResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcrsrConfigurationRecordersStatus'
--
-- * 'dcrsrStatus'
data DescribeConfigurationRecorderStatusResponse = DescribeConfigurationRecorderStatusResponse'
    { _dcrsrConfigurationRecordersStatus :: !(Maybe [ConfigurationRecorderStatus])
    , _dcrsrStatus                       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeConfigurationRecorderStatusResponse' smart constructor.
describeConfigurationRecorderStatusResponse :: Int -> DescribeConfigurationRecorderStatusResponse
describeConfigurationRecorderStatusResponse pStatus =
    DescribeConfigurationRecorderStatusResponse'
    { _dcrsrConfigurationRecordersStatus = Nothing
    , _dcrsrStatus = pStatus
    }

-- | A list that contains status of the specified recorders.
dcrsrConfigurationRecordersStatus :: Lens' DescribeConfigurationRecorderStatusResponse [ConfigurationRecorderStatus]
dcrsrConfigurationRecordersStatus = lens _dcrsrConfigurationRecordersStatus (\ s a -> s{_dcrsrConfigurationRecordersStatus = a}) . _Default;

-- | FIXME: Undocumented member.
dcrsrStatus :: Lens' DescribeConfigurationRecorderStatusResponse Int
dcrsrStatus = lens _dcrsrStatus (\ s a -> s{_dcrsrStatus = a});
