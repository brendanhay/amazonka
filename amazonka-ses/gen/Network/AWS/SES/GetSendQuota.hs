{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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

-- | Returns the user's current sending limits. This action is throttled at one
-- request per second. POST / HTTP/1.1 Date: Thu, 18 Aug 2011 22:22:36 GMT
-- Host: email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=W1YdiNOtf0jN3t7Lv63qhz7UZc3RrcmQpkGbopvnj/Y=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 94
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=GetSendQuota
-- &Timestamp=2011-08-18T22%3A22%3A36.000Z 127.0 200.0 1.0
-- 273021c6-c866-11e0-b926-699e21c3af9e.
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

import Network.AWS.Request.Query
import Network.AWS.SES.Types
import Network.AWS.Prelude

data GetSendQuota = GetSendQuota
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetSendQuota' request.
getSendQuota :: GetSendQuota
getSendQuota = GetSendQuota

instance ToQuery GetSendQuota where
    toQuery = genericQuery def

-- | Represents the user's current activity limits returned from a successful
-- GetSendQuota request.
data GetSendQuotaResponse = GetSendQuotaResponse
    { _gsqrMax24HourSend :: Maybe Double
    , _gsqrMaxSendRate :: Maybe Double
    , _gsqrSentLast24Hours :: Maybe Double
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetSendQuotaResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Max24HourSend ::@ @Maybe Double@
--
-- * @MaxSendRate ::@ @Maybe Double@
--
-- * @SentLast24Hours ::@ @Maybe Double@
--
getSendQuotaResponse :: GetSendQuotaResponse
getSendQuotaResponse = GetSendQuotaResponse
    { _gsqrMax24HourSend = Nothing
    , _gsqrMaxSendRate = Nothing
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

instance FromXML GetSendQuotaResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetSendQuota where
    type Sv GetSendQuota = SES
    type Rs GetSendQuota = GetSendQuotaResponse

    request = post "GetSendQuota"
    response _ = xmlResponse
