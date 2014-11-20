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

-- Module      : Network.AWS.CognitoIdentity.LookupDeveloperIdentity
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves the IdentityID associated with a DeveloperUserIdentifier or the
-- list of DeveloperUserIdentifiers associated with an IdentityId for an
-- existing identity. Either IdentityID or DeveloperUserIdentifier must not be
-- null. If you supply only one of these values, the other value will be
-- searched in the database and returned as a part of the response. If you
-- supply both, DeveloperUserIdentifier will be matched against IdentityID. If
-- the values are verified against the database, the response returns both
-- values and is the same as the request. Otherwise a
-- ResourceConflictException is thrown.
--
-- <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_LookupDeveloperIdentity.html>
module Network.AWS.CognitoIdentity.LookupDeveloperIdentity
    (
    -- * Request
      LookupDeveloperIdentity
    -- ** Request constructor
    , lookupDeveloperIdentity
    -- ** Request lenses
    , ldiDeveloperUserIdentifier
    , ldiIdentityId
    , ldiIdentityPoolId
    , ldiMaxResults
    , ldiNextToken

    -- * Response
    , LookupDeveloperIdentityResponse
    -- ** Response constructor
    , lookupDeveloperIdentityResponse
    -- ** Response lenses
    , ldirDeveloperUserIdentifierList
    , ldirIdentityId
    , ldirNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CognitoIdentity.Types
import qualified GHC.Exts

data LookupDeveloperIdentity = LookupDeveloperIdentity
    { _ldiDeveloperUserIdentifier :: Maybe Text
    , _ldiIdentityId              :: Maybe Text
    , _ldiIdentityPoolId          :: Text
    , _ldiMaxResults              :: Maybe Nat
    , _ldiNextToken               :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'LookupDeveloperIdentity' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldiDeveloperUserIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'ldiIdentityId' @::@ 'Maybe' 'Text'
--
-- * 'ldiIdentityPoolId' @::@ 'Text'
--
-- * 'ldiMaxResults' @::@ 'Maybe' 'Natural'
--
-- * 'ldiNextToken' @::@ 'Maybe' 'Text'
--
lookupDeveloperIdentity :: Text -- ^ 'ldiIdentityPoolId'
                        -> LookupDeveloperIdentity
lookupDeveloperIdentity p1 = LookupDeveloperIdentity
    { _ldiIdentityPoolId          = p1
    , _ldiIdentityId              = Nothing
    , _ldiDeveloperUserIdentifier = Nothing
    , _ldiMaxResults              = Nothing
    , _ldiNextToken               = Nothing
    }

-- | A unique ID used by your backend authentication process to identify a
-- user. Typically, a developer identity provider would issue many developer
-- user identifiers, in keeping with the number of users.
ldiDeveloperUserIdentifier :: Lens' LookupDeveloperIdentity (Maybe Text)
ldiDeveloperUserIdentifier =
    lens _ldiDeveloperUserIdentifier
        (\s a -> s { _ldiDeveloperUserIdentifier = a })

-- | A unique identifier in the format REGION:GUID.
ldiIdentityId :: Lens' LookupDeveloperIdentity (Maybe Text)
ldiIdentityId = lens _ldiIdentityId (\s a -> s { _ldiIdentityId = a })

-- | An identity pool ID in the format REGION:GUID.
ldiIdentityPoolId :: Lens' LookupDeveloperIdentity Text
ldiIdentityPoolId =
    lens _ldiIdentityPoolId (\s a -> s { _ldiIdentityPoolId = a })

-- | The maximum number of identities to return.
ldiMaxResults :: Lens' LookupDeveloperIdentity (Maybe Natural)
ldiMaxResults = lens _ldiMaxResults (\s a -> s { _ldiMaxResults = a }) . mapping _Nat

-- | A pagination token. The first call you make will have NextToken set to
-- null. After that the service will return NextToken values as needed. For
-- example, let's say you make a request with MaxResults set to 10, and
-- there are 20 matches in the database. The service will return a
-- pagination token as a part of the response. This token can be used to
-- call the API again and get results starting from the 11th match.
ldiNextToken :: Lens' LookupDeveloperIdentity (Maybe Text)
ldiNextToken = lens _ldiNextToken (\s a -> s { _ldiNextToken = a })

data LookupDeveloperIdentityResponse = LookupDeveloperIdentityResponse
    { _ldirDeveloperUserIdentifierList :: List "DeveloperUserIdentifierList" Text
    , _ldirIdentityId                  :: Maybe Text
    , _ldirNextToken                   :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'LookupDeveloperIdentityResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldirDeveloperUserIdentifierList' @::@ ['Text']
--
-- * 'ldirIdentityId' @::@ 'Maybe' 'Text'
--
-- * 'ldirNextToken' @::@ 'Maybe' 'Text'
--
lookupDeveloperIdentityResponse :: LookupDeveloperIdentityResponse
lookupDeveloperIdentityResponse = LookupDeveloperIdentityResponse
    { _ldirIdentityId                  = Nothing
    , _ldirDeveloperUserIdentifierList = mempty
    , _ldirNextToken                   = Nothing
    }

-- | This is the list of developer user identifiers associated with an
-- identity ID. Cognito supports the association of multiple developer user
-- identifiers with an identity ID.
ldirDeveloperUserIdentifierList :: Lens' LookupDeveloperIdentityResponse [Text]
ldirDeveloperUserIdentifierList =
    lens _ldirDeveloperUserIdentifierList
        (\s a -> s { _ldirDeveloperUserIdentifierList = a })
            . _List

-- | A unique identifier in the format REGION:GUID.
ldirIdentityId :: Lens' LookupDeveloperIdentityResponse (Maybe Text)
ldirIdentityId = lens _ldirIdentityId (\s a -> s { _ldirIdentityId = a })

-- | A pagination token. The first call you make will have NextToken set to
-- null. After that the service will return NextToken values as needed. For
-- example, let's say you make a request with MaxResults set to 10, and
-- there are 20 matches in the database. The service will return a
-- pagination token as a part of the response. This token can be used to
-- call the API again and get results starting from the 11th match.
ldirNextToken :: Lens' LookupDeveloperIdentityResponse (Maybe Text)
ldirNextToken = lens _ldirNextToken (\s a -> s { _ldirNextToken = a })

instance ToPath LookupDeveloperIdentity where
    toPath = const "/"

instance ToQuery LookupDeveloperIdentity where
    toQuery = const mempty

instance ToHeaders LookupDeveloperIdentity

instance ToJSON LookupDeveloperIdentity where
    toJSON LookupDeveloperIdentity{..} = object
        [ "IdentityPoolId"          .= _ldiIdentityPoolId
        , "IdentityId"              .= _ldiIdentityId
        , "DeveloperUserIdentifier" .= _ldiDeveloperUserIdentifier
        , "MaxResults"              .= _ldiMaxResults
        , "NextToken"               .= _ldiNextToken
        ]

instance AWSRequest LookupDeveloperIdentity where
    type Sv LookupDeveloperIdentity = CognitoIdentity
    type Rs LookupDeveloperIdentity = LookupDeveloperIdentityResponse

    request  = post "LookupDeveloperIdentity"
    response = jsonResponse

instance FromJSON LookupDeveloperIdentityResponse where
    parseJSON = withObject "LookupDeveloperIdentityResponse" $ \o -> LookupDeveloperIdentityResponse
        <$> o .:  "DeveloperUserIdentifierList"
        <*> o .:? "IdentityId"
        <*> o .:? "NextToken"
