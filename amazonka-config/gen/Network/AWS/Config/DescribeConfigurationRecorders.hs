{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Config.DescribeConfigurationRecorders
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

-- | Returns the name of one or more specified configuration recorders. If
-- the recorder name is not specified, this action returns the names of all
-- the configuration recorders associated with the account.
--
-- Currently, you can specify only one configuration recorder per account.
--
-- <http://docs.aws.amazon.com/config/latest/APIReference/API_DescribeConfigurationRecorders.html>
module Network.AWS.Config.DescribeConfigurationRecorders
    (
    -- * Request
      DescribeConfigurationRecorders
    -- ** Request constructor
    , describeConfigurationRecorders
    -- ** Request lenses
    , dcrConfigurationRecorderNames

    -- * Response
    , DescribeConfigurationRecordersResponse
    -- ** Response constructor
    , describeConfigurationRecordersResponse
    -- ** Response lenses
    , dcrrConfigurationRecorders
    , dcrrStatus
    ) where

import           Network.AWS.Config.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the DescribeConfigurationRecorders action.
--
-- /See:/ 'describeConfigurationRecorders' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcrConfigurationRecorderNames'
newtype DescribeConfigurationRecorders = DescribeConfigurationRecorders'
    { _dcrConfigurationRecorderNames :: Maybe [Text]
    } deriving (Eq,Read,Show)

-- | 'DescribeConfigurationRecorders' smart constructor.
describeConfigurationRecorders :: DescribeConfigurationRecorders
describeConfigurationRecorders =
    DescribeConfigurationRecorders'
    { _dcrConfigurationRecorderNames = Nothing
    }

-- | A list of configuration recorder names.
dcrConfigurationRecorderNames :: Lens' DescribeConfigurationRecorders [Text]
dcrConfigurationRecorderNames = lens _dcrConfigurationRecorderNames (\ s a -> s{_dcrConfigurationRecorderNames = a}) . _Default;

instance AWSRequest DescribeConfigurationRecorders
         where
        type Sv DescribeConfigurationRecorders = Config
        type Rs DescribeConfigurationRecorders =
             DescribeConfigurationRecordersResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeConfigurationRecordersResponse' <$>
                   (x .?> "ConfigurationRecorders" .!@ mempty) <*>
                     (pure s))

instance ToHeaders DescribeConfigurationRecorders
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.DescribeConfigurationRecorders"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeConfigurationRecorders where
        toJSON DescribeConfigurationRecorders'{..}
          = object
              ["ConfigurationRecorderNames" .=
                 _dcrConfigurationRecorderNames]

instance ToPath DescribeConfigurationRecorders where
        toPath = const "/"

instance ToQuery DescribeConfigurationRecorders where
        toQuery = const mempty

-- | The output for the DescribeConfigurationRecorders action.
--
-- /See:/ 'describeConfigurationRecordersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcrrConfigurationRecorders'
--
-- * 'dcrrStatus'
data DescribeConfigurationRecordersResponse = DescribeConfigurationRecordersResponse'
    { _dcrrConfigurationRecorders :: !(Maybe [ConfigurationRecorder])
    , _dcrrStatus                 :: !Status
    } deriving (Eq,Show)

-- | 'DescribeConfigurationRecordersResponse' smart constructor.
describeConfigurationRecordersResponse :: Status -> DescribeConfigurationRecordersResponse
describeConfigurationRecordersResponse pStatus =
    DescribeConfigurationRecordersResponse'
    { _dcrrConfigurationRecorders = Nothing
    , _dcrrStatus = pStatus
    }

-- | A list that contains the descriptions of the specified configuration
-- recorders.
dcrrConfigurationRecorders :: Lens' DescribeConfigurationRecordersResponse [ConfigurationRecorder]
dcrrConfigurationRecorders = lens _dcrrConfigurationRecorders (\ s a -> s{_dcrrConfigurationRecorders = a}) . _Default;

-- | FIXME: Undocumented member.
dcrrStatus :: Lens' DescribeConfigurationRecordersResponse Status
dcrrStatus = lens _dcrrStatus (\ s a -> s{_dcrrStatus = a});
