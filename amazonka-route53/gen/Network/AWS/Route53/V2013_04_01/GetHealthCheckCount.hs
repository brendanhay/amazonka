{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.V2013_04_01.GetHealthCheckCount
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | To retrieve a count of all your health checks, send a GET request to the
-- 2013-04-01/healthcheckcount resource.
module Network.AWS.Route53.V2013_04_01.GetHealthCheckCount
    (
    -- * Request
      GetHealthCheckCount
    -- ** Request constructor
    , mkGetHealthCheckCountRequest
    -- * Response
    , GetHealthCheckCountResponse
    -- ** Response lenses
    , ghccsHealthCheckCount
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.V2013_04_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetHealthCheckCount' request.
mkGetHealthCheckCountRequest :: GetHealthCheckCount
mkGetHealthCheckCountRequest = GetHealthCheckCount
{-# INLINE mkGetHealthCheckCountRequest #-}

    deriving (Eq, Show, Generic)

instance ToPath GetHealthCheckCount where
    toPath = const "/2013-04-01/healthcheckcount"

instance ToQuery GetHealthCheckCount

instance ToHeaders GetHealthCheckCount

instance ToXML GetHealthCheckCount where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetHealthCheckCountRequest"

newtype GetHealthCheckCountResponse = GetHealthCheckCountResponse
    { _ghccsHealthCheckCount :: Integer
      -- ^ The number of health checks associated with the current AWS
      -- account.
    } deriving (Show, Generic)

-- | The number of health checks associated with the current AWS account.
ghccsHealthCheckCount :: Lens' GetHealthCheckCountResponse (Integer)
ghccsHealthCheckCount = lens _ghccsHealthCheckCount (\s a -> s { _ghccsHealthCheckCount = a })
{-# INLINE ghccsHealthCheckCount #-}

instance FromXML GetHealthCheckCountResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetHealthCheckCount where
    type Sv GetHealthCheckCount = Route53
    type Rs GetHealthCheckCount = GetHealthCheckCountResponse

    request = get
    response _ = xmlResponse
