{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.Route53Domains.RetrieveDomainAuthCode
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation returns the AuthCode for the domain. To transfer a domain to
-- another registrar, you provide this value to the new registrar.
module Network.AWS.Route53Domains.RetrieveDomainAuthCode
    (
    -- * Request
      RetrieveDomainAuthCode
    -- ** Request constructor
    , retrieveDomainAuthCode
    -- ** Request lenses
    , rdacDomainName

    -- * Response
    , RetrieveDomainAuthCodeResponse
    -- ** Response constructor
    , retrieveDomainAuthCodeResponse
    -- ** Response lenses
    , rdacrAuthCode
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Route53Domains.Types

newtype RetrieveDomainAuthCode = RetrieveDomainAuthCode
    { _rdacDomainName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'RetrieveDomainAuthCode' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdacDomainName' @::@ 'Text'
--
retrieveDomainAuthCode :: Text -- ^ 'rdacDomainName'
                       -> RetrieveDomainAuthCode
retrieveDomainAuthCode p1 = RetrieveDomainAuthCode
    { _rdacDomainName = p1
    }

-- | The name of a domain. Type: String Default: None Constraints: The domain
-- name can contain only the letters a through z, the numbers 0 through 9,
-- and hyphen (-). Internationalized Domain Names are not supported.
-- Required: Yes.
rdacDomainName :: Lens' RetrieveDomainAuthCode Text
rdacDomainName = lens _rdacDomainName (\s a -> s { _rdacDomainName = a })

instance ToPath RetrieveDomainAuthCode where
    toPath = const "/"

instance ToQuery RetrieveDomainAuthCode where
    toQuery = const mempty

instance ToHeaders RetrieveDomainAuthCode

instance ToBody RetrieveDomainAuthCode where
    toBody = toBody . encode . _rdacDomainName

newtype RetrieveDomainAuthCodeResponse = RetrieveDomainAuthCodeResponse
    { _rdacrAuthCode :: Sensitive Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'RetrieveDomainAuthCodeResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdacrAuthCode' @::@ 'Text'
--
retrieveDomainAuthCodeResponse :: Text -- ^ 'rdacrAuthCode'
                               -> RetrieveDomainAuthCodeResponse
retrieveDomainAuthCodeResponse p1 = RetrieveDomainAuthCodeResponse
    { _rdacrAuthCode = withIso _Sensitive (const id) p1
    }

-- | The authorization code for the domain. Type: String.
rdacrAuthCode :: Lens' RetrieveDomainAuthCodeResponse Text
rdacrAuthCode = lens _rdacrAuthCode (\s a -> s { _rdacrAuthCode = a })
    . _Sensitive

instance AWSRequest RetrieveDomainAuthCode where
    type Sv RetrieveDomainAuthCode = Route53Domains
    type Rs RetrieveDomainAuthCode = RetrieveDomainAuthCodeResponse

    request  = post
    response = jsonResponse $ \h o -> RetrieveDomainAuthCodeResponse
        <$> o .: "AuthCode"
