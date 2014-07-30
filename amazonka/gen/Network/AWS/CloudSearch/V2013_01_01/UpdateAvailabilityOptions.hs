{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.V2013_01_01.UpdateAvailabilityOptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Configures the availability options for a domain. Enabling the Multi-AZ
-- option expands an Amazon CloudSearch domain to an additional Availability
-- Zone in the same Region to increase fault tolerance in the event of a
-- service disruption. Changes to the Multi-AZ option can take about half an
-- hour to become active. For more information, see Configuring Availability
-- Options in the Amazon CloudSearch Developer Guide.
module Network.AWS.CloudSearch.V2013_01_01.UpdateAvailabilityOptions where

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
import           Network.AWS.CloudSearch.V2013_01_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data UpdateAvailabilityOptions = UpdateAvailabilityOptions
    { _uaorMultiAZ :: Bool
      -- ^ You expand an existing search domain to a second Availability
      -- Zone by setting the Multi-AZ option to true. Similarly, you can
      -- turn off the Multi-AZ option to downgrade the domain to a single
      -- Availability Zone by setting the Multi-AZ option to false.
    , _uaorDomainName :: Text
      -- ^ A string that represents the name of a domain. Domain names are
      -- unique across the domains owned by an account within an AWS
      -- region. Domain names start with a letter or number and can
      -- contain the following characters: a-z (lowercase), 0-9, and -
      -- (hyphen).
    } deriving (Generic)

instance ToQuery UpdateAvailabilityOptions where
    toQuery = genericToQuery def

instance AWSRequest UpdateAvailabilityOptions where
    type Sv UpdateAvailabilityOptions = CloudSearch
    type Rs UpdateAvailabilityOptions = UpdateAvailabilityOptionsResponse

    request = post "UpdateAvailabilityOptions"
    response _ = xmlResponse

data UpdateAvailabilityOptionsResponse = UpdateAvailabilityOptionsResponse
    { _uaosAvailabilityOptions :: Maybe AvailabilityOptionsStatus
      -- ^ The newly-configured availability options. Indicates whether
      -- Multi-AZ is enabled for the domain.
    } deriving (Generic)

instance FromXML UpdateAvailabilityOptionsResponse where
    fromXMLOptions = xmlOptions
