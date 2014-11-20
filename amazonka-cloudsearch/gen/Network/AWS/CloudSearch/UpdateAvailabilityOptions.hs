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
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_UpdateAvailabilityOptions.html>
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

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudSearch.Types
import qualified GHC.Exts

data UpdateAvailabilityOptions = UpdateAvailabilityOptions
    { _uaoDomainName :: Text
    , _uaoMultiAZ    :: Bool
    } deriving (Eq, Ord, Show)

-- | 'UpdateAvailabilityOptions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uaoDomainName' @::@ 'Text'
--
-- * 'uaoMultiAZ' @::@ 'Bool'
--
updateAvailabilityOptions :: Text -- ^ 'uaoDomainName'
                          -> Bool -- ^ 'uaoMultiAZ'
                          -> UpdateAvailabilityOptions
updateAvailabilityOptions p1 p2 = UpdateAvailabilityOptions
    { _uaoDomainName = p1
    , _uaoMultiAZ    = p2
    }

uaoDomainName :: Lens' UpdateAvailabilityOptions Text
uaoDomainName = lens _uaoDomainName (\s a -> s { _uaoDomainName = a })

-- | You expand an existing search domain to a second Availability Zone by
-- setting the Multi-AZ option to true. Similarly, you can turn off the
-- Multi-AZ option to downgrade the domain to a single Availability Zone by
-- setting the Multi-AZ option to false.
uaoMultiAZ :: Lens' UpdateAvailabilityOptions Bool
uaoMultiAZ = lens _uaoMultiAZ (\s a -> s { _uaoMultiAZ = a })

newtype UpdateAvailabilityOptionsResponse = UpdateAvailabilityOptionsResponse
    { _uaorAvailabilityOptions :: Maybe AvailabilityOptionsStatus
    } deriving (Eq, Show)

-- | 'UpdateAvailabilityOptionsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uaorAvailabilityOptions' @::@ 'Maybe' 'AvailabilityOptionsStatus'
--
updateAvailabilityOptionsResponse :: UpdateAvailabilityOptionsResponse
updateAvailabilityOptionsResponse = UpdateAvailabilityOptionsResponse
    { _uaorAvailabilityOptions = Nothing
    }

-- | The newly-configured availability options. Indicates whether Multi-AZ is
-- enabled for the domain.
uaorAvailabilityOptions :: Lens' UpdateAvailabilityOptionsResponse (Maybe AvailabilityOptionsStatus)
uaorAvailabilityOptions =
    lens _uaorAvailabilityOptions (\s a -> s { _uaorAvailabilityOptions = a })

instance ToPath UpdateAvailabilityOptions where
    toPath = const "/"

instance ToQuery UpdateAvailabilityOptions where
    toQuery UpdateAvailabilityOptions{..} = mconcat
        [ "DomainName" =? _uaoDomainName
        , "MultiAZ"    =? _uaoMultiAZ
        ]

instance ToHeaders UpdateAvailabilityOptions

instance AWSRequest UpdateAvailabilityOptions where
    type Sv UpdateAvailabilityOptions = CloudSearch
    type Rs UpdateAvailabilityOptions = UpdateAvailabilityOptionsResponse

    request  = post "UpdateAvailabilityOptions"
    response = xmlResponse

instance FromXML UpdateAvailabilityOptionsResponse where
    parseXML = withElement "UpdateAvailabilityOptionsResult" $ \x -> UpdateAvailabilityOptionsResponse
        <$> x .@? "AvailabilityOptions"


Some kind of operator / class to check the types whether to continue?
