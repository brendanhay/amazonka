{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CognitoIdentity.V2014_06_30.GetOpenIdToken
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets an OpenID token, using a known Cognito ID. This known Cognito ID is
-- returned from GetId. You can optionally add additional logins for the
-- identity. Supplying multiple logins creates an implicit link.
-- GetOpenIdToken The following examples show a GetOpenIdToken request and
-- response, without the optional login values. { "IdentityId":
-- "us-east-1:852d4250-9eec-4006-8f84-4e82EXAMPLE3" } { "IdentityId":
-- "us-east-1:852d4250-9eec-4006-8f84-4e82EXAMPLE3", "Token":
-- "EXAmpLeTOkENUzUxMiIsInR5cCI6IkpXUyIsImtpZCI6InVzLWVhc3QtMTEifQ.eyJleHAiOjE0MDI2Njg0NTAsInN1YiI6InVzLWVhc3QtMTo5MjFhMzg0My0yZGQ2LTQ2YjgtYWIyZi1jNjc5NTUyZTM3MWUiLCJhdWQiOiJ1cy1lYXN0LTE6YWY0MzExY2EtODM1ZS00YjQ5LTgxNGMtMjI5MGQ4ZDU1YTZmIiwiaXNzIjoiaHR0cHM6Ly9jb2duaXRvLWlkZW50aXR5LXB1YmxpYy1pYWQtYmV0YS5hbWF6b24uY29tIiwiaWF0IjoxNDAyNjY3ODUwLCJhbXIiOlsidW5hdXRoZW50aWNhdGVkIl19.faWdRGsKxT8YqTBnAow1fNyXE57kjScKQ0lyFpFAUIl6VNVV-nQ_QD8XKHB_pAY2UNtxYFDoGhHrL3cqH_FLSfRLG-X3EaIrCsr0A6KIW7X69wsAxJQB-EvYru0UhDpcPaDyQUXTHFmRP9bzJMsSLi7nXm-OD4DCujX3vKwOhlSymbH9KbAG105t3_G_a8tsUbCV488nvlrA-7Omp0D18T1__XeZttldW1GODOK2OY2bK5-3eyodcqbVXaPTotxO5PTlYpzuMS1XfTejC8LJ2DocP_eBT7xhSr2qkro9Y6uCDph_-6ttYrXRaaLKZv3v1Lz6PGHrsPhJdK_bYRHhdg"
-- }.
module Network.AWS.CognitoIdentity.V2014_06_30.GetOpenIdToken
    (
    -- * Request
      GetOpenIdToken
    -- ** Request constructor
    , mkGetOpenIdTokenInput
    -- ** Request lenses
    , goitiIdentityId
    , goitiLogins

    -- * Response
    , GetOpenIdTokenResponse
    -- ** Response lenses
    , goitrIdentityId
    , goitrToken
    ) where

import           Network.AWS.CognitoIdentity.V2014_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetOpenIdToken' request.
mkGetOpenIdTokenInput :: Text -- ^ 'goitiIdentityId'
                      -> GetOpenIdToken
mkGetOpenIdTokenInput p1 = GetOpenIdToken
    { _goitiIdentityId = p1
    , _goitiLogins = mempty
    }
{-# INLINE mkGetOpenIdTokenInput #-}

data GetOpenIdToken = GetOpenIdToken
    { _goitiIdentityId :: Text
      -- ^ A unique identifier in the format REGION:GUID.
    , _goitiLogins :: Map Text Text
      -- ^ A set of optional name/value pairs that map provider names to
      -- provider tokens.
    } deriving (Show, Generic)

-- | A unique identifier in the format REGION:GUID.
goitiIdentityId :: Lens' GetOpenIdToken (Text)
goitiIdentityId = lens _goitiIdentityId (\s a -> s { _goitiIdentityId = a })
{-# INLINE goitiIdentityId #-}

-- | A set of optional name/value pairs that map provider names to provider
-- tokens.
goitiLogins :: Lens' GetOpenIdToken (Map Text Text)
goitiLogins = lens _goitiLogins (\s a -> s { _goitiLogins = a })
{-# INLINE goitiLogins #-}

instance ToPath GetOpenIdToken

instance ToQuery GetOpenIdToken

instance ToHeaders GetOpenIdToken

instance ToJSON GetOpenIdToken

data GetOpenIdTokenResponse = GetOpenIdTokenResponse
    { _goitrIdentityId :: Maybe Text
      -- ^ A unique identifier in the format REGION:GUID. Note that the
      -- IdentityId returned may not match the one passed on input.
    , _goitrToken :: Maybe Text
      -- ^ An OpenID token.
    } deriving (Show, Generic)

-- | A unique identifier in the format REGION:GUID. Note that the IdentityId
-- returned may not match the one passed on input.
goitrIdentityId :: Lens' GetOpenIdTokenResponse (Maybe Text)
goitrIdentityId = lens _goitrIdentityId (\s a -> s { _goitrIdentityId = a })
{-# INLINE goitrIdentityId #-}

-- | An OpenID token.
goitrToken :: Lens' GetOpenIdTokenResponse (Maybe Text)
goitrToken = lens _goitrToken (\s a -> s { _goitrToken = a })
{-# INLINE goitrToken #-}

instance FromJSON GetOpenIdTokenResponse

instance AWSRequest GetOpenIdToken where
    type Sv GetOpenIdToken = CognitoIdentity
    type Rs GetOpenIdToken = GetOpenIdTokenResponse

    request = get
    response _ = jsonResponse
