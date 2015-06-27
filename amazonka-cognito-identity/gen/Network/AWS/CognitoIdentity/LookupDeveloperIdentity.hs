{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CognitoIdentity.LookupDeveloperIdentity
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

-- | Retrieves the @IdentityID@ associated with a @DeveloperUserIdentifier@
-- or the list of @DeveloperUserIdentifier@s associated with an
-- @IdentityId@ for an existing identity. Either @IdentityID@ or
-- @DeveloperUserIdentifier@ must not be null. If you supply only one of
-- these values, the other value will be searched in the database and
-- returned as a part of the response. If you supply both,
-- @DeveloperUserIdentifier@ will be matched against @IdentityID@. If the
-- values are verified against the database, the response returns both
-- values and is the same as the request. Otherwise a
-- @ResourceConflictException@ is thrown.
--
-- You must use AWS Developer credentials to call this API.
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
    , ldiNextToken
    , ldiIdentityId
    , ldiMaxResults
    , ldiIdentityPoolId

    -- * Response
    , LookupDeveloperIdentityResponse
    -- ** Response constructor
    , lookupDeveloperIdentityResponse
    -- ** Response lenses
    , ldirNextToken
    , ldirIdentityId
    , ldirDeveloperUserIdentifierList
    , ldirStatus
    ) where

import           Network.AWS.CognitoIdentity.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Input to the @LookupDeveloperIdentityInput@ action.
--
-- /See:/ 'lookupDeveloperIdentity' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldiDeveloperUserIdentifier'
--
-- * 'ldiNextToken'
--
-- * 'ldiIdentityId'
--
-- * 'ldiMaxResults'
--
-- * 'ldiIdentityPoolId'
data LookupDeveloperIdentity = LookupDeveloperIdentity'
    { _ldiDeveloperUserIdentifier :: Maybe Text
    , _ldiNextToken               :: Maybe Text
    , _ldiIdentityId              :: Maybe Text
    , _ldiMaxResults              :: Maybe Nat
    , _ldiIdentityPoolId          :: Text
    } deriving (Eq,Read,Show)

-- | 'LookupDeveloperIdentity' smart constructor.
lookupDeveloperIdentity :: Text -> LookupDeveloperIdentity
lookupDeveloperIdentity pIdentityPoolId =
    LookupDeveloperIdentity'
    { _ldiDeveloperUserIdentifier = Nothing
    , _ldiNextToken = Nothing
    , _ldiIdentityId = Nothing
    , _ldiMaxResults = Nothing
    , _ldiIdentityPoolId = pIdentityPoolId
    }

-- | A unique ID used by your backend authentication process to identify a
-- user. Typically, a developer identity provider would issue many
-- developer user identifiers, in keeping with the number of users.
ldiDeveloperUserIdentifier :: Lens' LookupDeveloperIdentity (Maybe Text)
ldiDeveloperUserIdentifier = lens _ldiDeveloperUserIdentifier (\ s a -> s{_ldiDeveloperUserIdentifier = a});

-- | A pagination token. The first call you make will have @NextToken@ set to
-- null. After that the service will return @NextToken@ values as needed.
-- For example, let\'s say you make a request with @MaxResults@ set to 10,
-- and there are 20 matches in the database. The service will return a
-- pagination token as a part of the response. This token can be used to
-- call the API again and get results starting from the 11th match.
ldiNextToken :: Lens' LookupDeveloperIdentity (Maybe Text)
ldiNextToken = lens _ldiNextToken (\ s a -> s{_ldiNextToken = a});

-- | A unique identifier in the format REGION:GUID.
ldiIdentityId :: Lens' LookupDeveloperIdentity (Maybe Text)
ldiIdentityId = lens _ldiIdentityId (\ s a -> s{_ldiIdentityId = a});

-- | The maximum number of identities to return.
ldiMaxResults :: Lens' LookupDeveloperIdentity (Maybe Natural)
ldiMaxResults = lens _ldiMaxResults (\ s a -> s{_ldiMaxResults = a}) . mapping _Nat;

-- | An identity pool ID in the format REGION:GUID.
ldiIdentityPoolId :: Lens' LookupDeveloperIdentity Text
ldiIdentityPoolId = lens _ldiIdentityPoolId (\ s a -> s{_ldiIdentityPoolId = a});

instance AWSRequest LookupDeveloperIdentity where
        type Sv LookupDeveloperIdentity = CognitoIdentity
        type Rs LookupDeveloperIdentity =
             LookupDeveloperIdentityResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 LookupDeveloperIdentityResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "IdentityId") <*>
                     (x .?> "DeveloperUserIdentifierList" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders LookupDeveloperIdentity where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityService.LookupDeveloperIdentity"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON LookupDeveloperIdentity where
        toJSON LookupDeveloperIdentity'{..}
          = object
              ["DeveloperUserIdentifier" .=
                 _ldiDeveloperUserIdentifier,
               "NextToken" .= _ldiNextToken,
               "IdentityId" .= _ldiIdentityId,
               "MaxResults" .= _ldiMaxResults,
               "IdentityPoolId" .= _ldiIdentityPoolId]

instance ToPath LookupDeveloperIdentity where
        toPath = const "/"

instance ToQuery LookupDeveloperIdentity where
        toQuery = const mempty

-- | Returned in response to a successful @LookupDeveloperIdentity@ action.
--
-- /See:/ 'lookupDeveloperIdentityResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldirNextToken'
--
-- * 'ldirIdentityId'
--
-- * 'ldirDeveloperUserIdentifierList'
--
-- * 'ldirStatus'
data LookupDeveloperIdentityResponse = LookupDeveloperIdentityResponse'
    { _ldirNextToken                   :: Maybe Text
    , _ldirIdentityId                  :: Maybe Text
    , _ldirDeveloperUserIdentifierList :: Maybe [Text]
    , _ldirStatus                      :: !Int
    } deriving (Eq,Read,Show)

-- | 'LookupDeveloperIdentityResponse' smart constructor.
lookupDeveloperIdentityResponse :: Int -> LookupDeveloperIdentityResponse
lookupDeveloperIdentityResponse pStatus =
    LookupDeveloperIdentityResponse'
    { _ldirNextToken = Nothing
    , _ldirIdentityId = Nothing
    , _ldirDeveloperUserIdentifierList = Nothing
    , _ldirStatus = pStatus
    }

-- | A pagination token. The first call you make will have @NextToken@ set to
-- null. After that the service will return @NextToken@ values as needed.
-- For example, let\'s say you make a request with @MaxResults@ set to 10,
-- and there are 20 matches in the database. The service will return a
-- pagination token as a part of the response. This token can be used to
-- call the API again and get results starting from the 11th match.
ldirNextToken :: Lens' LookupDeveloperIdentityResponse (Maybe Text)
ldirNextToken = lens _ldirNextToken (\ s a -> s{_ldirNextToken = a});

-- | A unique identifier in the format REGION:GUID.
ldirIdentityId :: Lens' LookupDeveloperIdentityResponse (Maybe Text)
ldirIdentityId = lens _ldirIdentityId (\ s a -> s{_ldirIdentityId = a});

-- | This is the list of developer user identifiers associated with an
-- identity ID. Cognito supports the association of multiple developer user
-- identifiers with an identity ID.
ldirDeveloperUserIdentifierList :: Lens' LookupDeveloperIdentityResponse [Text]
ldirDeveloperUserIdentifierList = lens _ldirDeveloperUserIdentifierList (\ s a -> s{_ldirDeveloperUserIdentifierList = a}) . _Default;

-- | FIXME: Undocumented member.
ldirStatus :: Lens' LookupDeveloperIdentityResponse Int
ldirStatus = lens _ldirStatus (\ s a -> s{_ldirStatus = a});
