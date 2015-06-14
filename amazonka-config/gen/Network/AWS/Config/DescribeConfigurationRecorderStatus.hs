{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Config.DescribeConfigurationRecorderStatus
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

-- | Returns the current status of the specified configuration recorder. If a
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
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.Config.Types

-- | /See:/ 'describeConfigurationRecorderStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcrsConfigurationRecorderNames'
newtype DescribeConfigurationRecorderStatus = DescribeConfigurationRecorderStatus'{_dcrsConfigurationRecorderNames :: Maybe [Text]} deriving (Eq, Read, Show)

-- | 'DescribeConfigurationRecorderStatus' smart constructor.
describeConfigurationRecorderStatus :: DescribeConfigurationRecorderStatus
describeConfigurationRecorderStatus = DescribeConfigurationRecorderStatus'{_dcrsConfigurationRecorderNames = Nothing};

-- | The name(s) of the configuration recorder. If the name is not specified,
-- the action returns the current status of all the configuration recorders
-- associated with the account.
dcrsConfigurationRecorderNames :: Lens' DescribeConfigurationRecorderStatus (Maybe [Text])
dcrsConfigurationRecorderNames = lens _dcrsConfigurationRecorderNames (\ s a -> s{_dcrsConfigurationRecorderNames = a});

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
                   x .?> "ConfigurationRecordersStatus" .!@ mempty)

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

-- | /See:/ 'describeConfigurationRecorderStatusResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcrsrConfigurationRecordersStatus'
newtype DescribeConfigurationRecorderStatusResponse = DescribeConfigurationRecorderStatusResponse'{_dcrsrConfigurationRecordersStatus :: Maybe [ConfigurationRecorderStatus]} deriving (Eq, Read, Show)

-- | 'DescribeConfigurationRecorderStatusResponse' smart constructor.
describeConfigurationRecorderStatusResponse :: DescribeConfigurationRecorderStatusResponse
describeConfigurationRecorderStatusResponse = DescribeConfigurationRecorderStatusResponse'{_dcrsrConfigurationRecordersStatus = Nothing};

-- | A list that contains status of the specified recorders.
dcrsrConfigurationRecordersStatus :: Lens' DescribeConfigurationRecorderStatusResponse (Maybe [ConfigurationRecorderStatus])
dcrsrConfigurationRecordersStatus = lens _dcrsrConfigurationRecordersStatus (\ s a -> s{_dcrsrConfigurationRecordersStatus = a});
