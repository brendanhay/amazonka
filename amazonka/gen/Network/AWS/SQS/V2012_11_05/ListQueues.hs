{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.V2012_11_05.ListQueues
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of your queues. The maximum number of queues that can be
-- returned is 1000. If you specify a value for the optional QueueNamePrefix
-- parameter, only queues with a name beginning with the specified value are
-- returned. The following example Query request returns the queues whose
-- names begin with the letter "T". http://sqs.us-east-1.amazonaws.com/
-- ?Action=ListQueues &QueueNamePrefix=t &Version=2009-02-01
-- &SignatureMethod=HmacSHA256 &Expires=2009-04-18T22%3A52%3A43PST
-- &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &SignatureVersion=2
-- &Signature=Dqlp3Sd6ljTUA9Uf6SGtEExwUQEXAMPLE
-- http://sqs.us-east-1.amazonaws.com/123456789012/testQueue
-- 725275ae-0b9b-4762-b238-436d7c65a1ac.
module Network.AWS.SQS.V2012_11_05.ListQueues where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.SQS.V2012_11_05.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Minimum specification for a 'ListQueues' request.
listQueues :: ListQueues
listQueues = ListQueues
    { _lqrQueueNamePrefix = Nothing
    }

data ListQueues = ListQueues
    { _lqrQueueNamePrefix :: Maybe Text
      -- ^ A string to use for filtering the list results. Only those queues
      -- whose name begins with the specified string are returned.
    } deriving (Generic)

instance ToQuery ListQueues where
    toQuery = genericToQuery def

instance AWSRequest ListQueues where
    type Sv ListQueues = SQS
    type Rs ListQueues = ListQueuesResponse

    request = post "ListQueues"
    response _ = xmlResponse

data ListQueuesResponse = ListQueuesResponse
    { _lqsQueueUrls :: [Text]
      -- ^ A list of queue URLs, up to 1000 entries.
    } deriving (Generic)

instance FromXML ListQueuesResponse where
    fromXMLOptions = xmlOptions
