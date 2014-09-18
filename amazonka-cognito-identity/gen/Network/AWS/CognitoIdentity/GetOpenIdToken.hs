{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CognitoIdentity.GetOpenIdToken
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
module Network.AWS.CognitoIdentity.GetOpenIdToken
    (
    -- * Request
      GetOpenIdToken
    -- ** Request constructor
    , getOpenIdToken
    -- ** Request lenses
    , goitIdentityId
    , goitLogins

    -- * Response
    , GetOpenIdTokenResponse
    -- ** Response constructor
    , getOpenIdTokenResponse
    -- ** Response lenses
    , goitrIdentityId
    , goitrToken
    ) where

import Network.AWS.CognitoIdentity.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Input to the GetOpenIdToken action.
data GetOpenIdToken = GetOpenIdToken
    { _goitIdentityId :: Text
    , _goitLogins :: Map Text Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetOpenIdToken' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @IdentityId ::@ @Text@
--
-- * @Logins ::@ @Map Text Text@
--
getOpenIdToken :: Text -- ^ 'goitIdentityId'
                 -> GetOpenIdToken
getOpenIdToken p1 = GetOpenIdToken
    { _goitIdentityId = p1
    , _goitLogins = mempty
    }

-- | A unique identifier in the format REGION:GUID.
goitIdentityId :: Lens' GetOpenIdToken Text
goitIdentityId = lens _goitIdentityId (\s a -> s { _goitIdentityId = a })

-- | A set of optional name/value pairs that map provider names to provider
-- tokens.
goitLogins :: Lens' GetOpenIdToken (Map Text Text)
goitLogins = lens _goitLogins (\s a -> s { _goitLogins = a })

instance ToPath GetOpenIdToken

instance ToQuery GetOpenIdToken

instance ToHeaders GetOpenIdToken

instance ToJSON GetOpenIdToken

-- | Returned in response to a successful GetOpenIdToken request.
data GetOpenIdTokenResponse = GetOpenIdTokenResponse
    { _goitrIdentityId :: Maybe Text
    , _goitrToken :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetOpenIdTokenResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @IdentityId ::@ @Maybe Text@
--
-- * @Token ::@ @Maybe Text@
--
getOpenIdTokenResponse :: GetOpenIdTokenResponse
getOpenIdTokenResponse = GetOpenIdTokenResponse
    { _goitrIdentityId = Nothing
    , _goitrToken = Nothing
    }

-- | A unique identifier in the format REGION:GUID. Note that the IdentityId
-- returned may not match the one passed on input.
goitrIdentityId :: Lens' GetOpenIdTokenResponse (Maybe Text)
goitrIdentityId = lens _goitrIdentityId (\s a -> s { _goitrIdentityId = a })

-- | An OpenID token.
goitrToken :: Lens' GetOpenIdTokenResponse (Maybe Text)
goitrToken = lens _goitrToken (\s a -> s { _goitrToken = a })

instance FromJSON GetOpenIdTokenResponse

instance AWSRequest GetOpenIdToken where
    type Sv GetOpenIdToken = CognitoIdentity
    type Rs GetOpenIdToken = GetOpenIdTokenResponse

    request = get
    response _ = jsonResponse
