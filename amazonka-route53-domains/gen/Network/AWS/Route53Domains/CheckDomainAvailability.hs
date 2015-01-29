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

-- Module      : Network.AWS.Route53Domains.CheckDomainAvailability
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

-- | This operation checks the availability of one domain name. You can access
-- this API without authenticating. Note that if the availability status of a
-- domain is pending, you must submit another request to determine the
-- availability of the domain name.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/api-CheckDomainAvailability.html>
module Network.AWS.Route53Domains.CheckDomainAvailability
    (
    -- * Request
      CheckDomainAvailability
    -- ** Request constructor
    , checkDomainAvailability
    -- ** Request lenses
    , cdaDomainName
    , cdaIdnLangCode

    -- * Response
    , CheckDomainAvailabilityResponse
    -- ** Response constructor
    , checkDomainAvailabilityResponse
    -- ** Response lenses
    , cdarAvailability
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Route53Domains.Types
import qualified GHC.Exts

data CheckDomainAvailability = CheckDomainAvailability
    { _cdaDomainName  :: Text
    , _cdaIdnLangCode :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'CheckDomainAvailability' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdaDomainName' @::@ 'Text'
--
-- * 'cdaIdnLangCode' @::@ 'Maybe' 'Text'
--
checkDomainAvailability :: Text -- ^ 'cdaDomainName'
                        -> CheckDomainAvailability
checkDomainAvailability p1 = CheckDomainAvailability
    { _cdaDomainName  = p1
    , _cdaIdnLangCode = Nothing
    }

-- | The name of a domain.
--
-- Type: String
--
-- Default: None
--
-- Constraints: The domain name can contain only the letters a through z, the
-- numbers 0 through 9, and hyphen (-). Internationalized Domain Names are not
-- supported.
--
-- Required: Yes
cdaDomainName :: Lens' CheckDomainAvailability Text
cdaDomainName = lens _cdaDomainName (\s a -> s { _cdaDomainName = a })

-- | Reserved for future use.
cdaIdnLangCode :: Lens' CheckDomainAvailability (Maybe Text)
cdaIdnLangCode = lens _cdaIdnLangCode (\s a -> s { _cdaIdnLangCode = a })

newtype CheckDomainAvailabilityResponse = CheckDomainAvailabilityResponse
    { _cdarAvailability :: DomainAvailability
    } deriving (Eq, Read, Show)

-- | 'CheckDomainAvailabilityResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdarAvailability' @::@ 'DomainAvailability'
--
checkDomainAvailabilityResponse :: DomainAvailability -- ^ 'cdarAvailability'
                                -> CheckDomainAvailabilityResponse
checkDomainAvailabilityResponse p1 = CheckDomainAvailabilityResponse
    { _cdarAvailability = p1
    }

-- | Whether the domain name is available for registering.
--
-- Type: String
--
-- Valid values:
--
-- 'AVAILABLE' – The domain name is available.  'AVAILABLE_RESERVED' – The domain
-- name is reserved under specific conditions.  'AVAILABLE_PREORDER' – The domain
-- name is available and can be preordered.  'UNAVAILABLE' – The domain name is
-- not available.  'UNAVAILABLE_PREMIUM' – The domain name is not available.  'UNAVAILABLE_RESTRICTED' – The domain name is forbidden.  'RESERVED' – The domain name has been
-- reserved for another person or organization.
cdarAvailability :: Lens' CheckDomainAvailabilityResponse DomainAvailability
cdarAvailability = lens _cdarAvailability (\s a -> s { _cdarAvailability = a })

instance ToPath CheckDomainAvailability where
    toPath = const "/"

instance ToQuery CheckDomainAvailability where
    toQuery = const mempty

instance ToHeaders CheckDomainAvailability

instance ToJSON CheckDomainAvailability where
    toJSON CheckDomainAvailability{..} = object
        [ "DomainName"  .= _cdaDomainName
        , "IdnLangCode" .= _cdaIdnLangCode
        ]

instance AWSRequest CheckDomainAvailability where
    type Sv CheckDomainAvailability = Route53Domains
    type Rs CheckDomainAvailability = CheckDomainAvailabilityResponse

    request  = post "CheckDomainAvailability"
    response = jsonResponse

instance FromJSON CheckDomainAvailabilityResponse where
    parseJSON = withObject "CheckDomainAvailabilityResponse" $ \o -> CheckDomainAvailabilityResponse
        <$> o .:  "Availability"
