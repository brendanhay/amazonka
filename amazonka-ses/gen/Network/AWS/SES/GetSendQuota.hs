{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.GetSendQuota
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns the user's current sending limits.
--
-- This action is throttled at one request per second.
--
-- <http://docs.aws.amazon.com/ses/latest/APIReference/API_GetSendQuota.html>
module Network.AWS.SES.GetSendQuota
    (
    -- * Request
      GetSendQuota
    -- ** Request constructor
    , getSendQuota

    -- * Response
    , GetSendQuotaResponse
    -- ** Response constructor
    , getSendQuotaResponse
    -- ** Response lenses
    , gsqrMax24HourSend
    , gsqrMaxSendRate
    , gsqrSentLast24Hours
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SES.Types
import qualified GHC.Exts

data GetSendQuota = GetSendQuota
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'GetSendQuota' constructor.
getSendQuota :: GetSendQuota
getSendQuota = GetSendQuota

data GetSendQuotaResponse = GetSendQuotaResponse
    { _gsqrMax24HourSend   :: Maybe Double
    , _gsqrMaxSendRate     :: Maybe Double
    , _gsqrSentLast24Hours :: Maybe Double
    } deriving (Eq, Ord, Read, Show)

-- | 'GetSendQuotaResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsqrMax24HourSend' @::@ 'Maybe' 'Double'
--
-- * 'gsqrMaxSendRate' @::@ 'Maybe' 'Double'
--
-- * 'gsqrSentLast24Hours' @::@ 'Maybe' 'Double'
--
getSendQuotaResponse :: GetSendQuotaResponse
getSendQuotaResponse = GetSendQuotaResponse
    { _gsqrMax24HourSend   = Nothing
    , _gsqrMaxSendRate     = Nothing
    , _gsqrSentLast24Hours = Nothing
    }

-- | The maximum number of emails the user is allowed to send in a 24-hour
-- interval.
gsqrMax24HourSend :: Lens' GetSendQuotaResponse (Maybe Double)
gsqrMax24HourSend =
    lens _gsqrMax24HourSend (\s a -> s { _gsqrMax24HourSend = a })

-- | The maximum number of emails the user is allowed to send per second.
gsqrMaxSendRate :: Lens' GetSendQuotaResponse (Maybe Double)
gsqrMaxSendRate = lens _gsqrMaxSendRate (\s a -> s { _gsqrMaxSendRate = a })

-- | The number of emails sent during the previous 24 hours.
gsqrSentLast24Hours :: Lens' GetSendQuotaResponse (Maybe Double)
gsqrSentLast24Hours =
    lens _gsqrSentLast24Hours (\s a -> s { _gsqrSentLast24Hours = a })

instance ToPath GetSendQuota where
    toPath = const "/"

instance ToQuery GetSendQuota where
    toQuery = const mempty

instance ToHeaders GetSendQuota

instance AWSRequest GetSendQuota where
    type Sv GetSendQuota = SES
    type Rs GetSendQuota = GetSendQuotaResponse

    request  = post "GetSendQuota"
    response = xmlResponse

instance FromXML GetSendQuotaResponse where
    parseXML = withElement "GetSendQuotaResult" $ \x -> GetSendQuotaResponse
        <$> x .@? "Max24HourSend"
        <*> x .@? "MaxSendRate"
        <*> x .@? "SentLast24Hours"
