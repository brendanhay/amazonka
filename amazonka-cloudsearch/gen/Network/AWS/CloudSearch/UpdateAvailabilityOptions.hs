{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.UpdateAvailabilityOptions
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
module Network.AWS.CloudSearch.UpdateAvailabilityOptions
    (
    -- * Request
      UpdateAvailabilityOptions
    -- ** Request constructor
    , updateAvailabilityOptions
    -- ** Request lenses
    , uaoDomainName
    , uaoMultiAZ

    -- * Response
    , UpdateAvailabilityOptionsResponse
    -- ** Response constructor
    , updateAvailabilityOptionsResponse
    -- ** Response lenses
    , uaorAvailabilityOptions
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.Types
import Network.AWS.Prelude

-- | Container for the parameters to the UpdateAvailabilityOptions operation.
-- Specifies the name of the domain you want to update and the Multi-AZ
-- availability option.
data UpdateAvailabilityOptions = UpdateAvailabilityOptions
    { _uaoDomainName :: Text
    , _uaoMultiAZ :: !Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateAvailabilityOptions' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DomainName ::@ @Text@
--
-- * @MultiAZ ::@ @Bool@
--
updateAvailabilityOptions :: Text -- ^ 'uaoDomainName'
                            -> Bool -- ^ 'uaoMultiAZ'
                            -> UpdateAvailabilityOptions
updateAvailabilityOptions p1 p2 = UpdateAvailabilityOptions
    { _uaoDomainName = p1
    , _uaoMultiAZ = p2
    }

-- | A string that represents the name of a domain. Domain names are unique
-- across the domains owned by an account within an AWS region. Domain names
-- start with a letter or number and can contain the following characters: a-z
-- (lowercase), 0-9, and - (hyphen).
uaoDomainName :: Lens' UpdateAvailabilityOptions Text
uaoDomainName = lens _uaoDomainName (\s a -> s { _uaoDomainName = a })

-- | You expand an existing search domain to a second Availability Zone by
-- setting the Multi-AZ option to true. Similarly, you can turn off the
-- Multi-AZ option to downgrade the domain to a single Availability Zone by
-- setting the Multi-AZ option to false.
uaoMultiAZ :: Lens' UpdateAvailabilityOptions Bool
uaoMultiAZ = lens _uaoMultiAZ (\s a -> s { _uaoMultiAZ = a })

instance ToQuery UpdateAvailabilityOptions where
    toQuery = genericQuery def

-- | The result of a UpdateAvailabilityOptions request. Contains the status of
-- the domain's availability options.
newtype UpdateAvailabilityOptionsResponse = UpdateAvailabilityOptionsResponse
    { _uaorAvailabilityOptions :: Maybe AvailabilityOptionsStatus
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateAvailabilityOptionsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AvailabilityOptions ::@ @Maybe AvailabilityOptionsStatus@
--
updateAvailabilityOptionsResponse :: UpdateAvailabilityOptionsResponse
updateAvailabilityOptionsResponse = UpdateAvailabilityOptionsResponse
    { _uaorAvailabilityOptions = Nothing
    }

-- | The newly-configured availability options. Indicates whether Multi-AZ is
-- enabled for the domain.
uaorAvailabilityOptions :: Lens' UpdateAvailabilityOptionsResponse (Maybe AvailabilityOptionsStatus)
uaorAvailabilityOptions =
    lens _uaorAvailabilityOptions
         (\s a -> s { _uaorAvailabilityOptions = a })

instance FromXML UpdateAvailabilityOptionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest UpdateAvailabilityOptions where
    type Sv UpdateAvailabilityOptions = CloudSearch
    type Rs UpdateAvailabilityOptions = UpdateAvailabilityOptionsResponse

    request = post "UpdateAvailabilityOptions"
    response _ = xmlResponse
