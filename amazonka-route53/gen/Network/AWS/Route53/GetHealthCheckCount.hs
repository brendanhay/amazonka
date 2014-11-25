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

-- Module      : Network.AWS.Route53.GetHealthCheckCount
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | To retrieve a count of all your health checks, send a 'GET' request to the '2013-04-01/healthcheckcount' resource.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_GetHealthCheckCount.html>
module Network.AWS.Route53.GetHealthCheckCount
    (
    -- * Request
      GetHealthCheckCount
    -- ** Request constructor
    , getHealthCheckCount

    -- * Response
    , GetHealthCheckCountResponse
    -- ** Response constructor
    , getHealthCheckCountResponse
    -- ** Response lenses
    , ghccrHealthCheckCount
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.Route53.Types
import qualified GHC.Exts

data GetHealthCheckCount = GetHealthCheckCount
    deriving (Eq, Ord, Show, Generic)

-- | 'GetHealthCheckCount' constructor.
getHealthCheckCount :: GetHealthCheckCount
getHealthCheckCount = GetHealthCheckCount

newtype GetHealthCheckCountResponse = GetHealthCheckCountResponse
    { _ghccrHealthCheckCount :: Integer
    } deriving (Eq, Ord, Show, Enum, Num, Integral, Real)

-- | 'GetHealthCheckCountResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ghccrHealthCheckCount' @::@ 'Integer'
--
getHealthCheckCountResponse :: Integer -- ^ 'ghccrHealthCheckCount'
                            -> GetHealthCheckCountResponse
getHealthCheckCountResponse p1 = GetHealthCheckCountResponse
    { _ghccrHealthCheckCount = p1
    }

-- | The number of health checks associated with the current AWS account.
--
ghccrHealthCheckCount :: Lens' GetHealthCheckCountResponse Integer
ghccrHealthCheckCount =
    lens _ghccrHealthCheckCount (\s a -> s { _ghccrHealthCheckCount = a })

instance ToPath GetHealthCheckCount where
    toPath = const "/2013-04-01/healthcheckcount"

instance ToQuery GetHealthCheckCount where
    toQuery = const mempty

instance ToHeaders GetHealthCheckCount

instance ToXMLRoot GetHealthCheckCount where
    toXMLRoot = const (namespaced ns "GetHealthCheckCount" [])

instance ToXML GetHealthCheckCount

instance AWSRequest GetHealthCheckCount where
    type Sv GetHealthCheckCount = Route53
    type Rs GetHealthCheckCount = GetHealthCheckCountResponse

    request  = get
    response = xmlResponse

instance FromXML GetHealthCheckCountResponse where
    parseXML x = GetHealthCheckCountResponse
        <$> x .@  "HealthCheckCount"
