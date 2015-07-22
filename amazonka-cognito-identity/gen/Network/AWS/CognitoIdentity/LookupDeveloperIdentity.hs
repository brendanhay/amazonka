{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.LookupDeveloperIdentity
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the @IdentityID@ associated with a @DeveloperUserIdentifier@
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
    , ldirqDeveloperUserIdentifier
    , ldirqNextToken
    , ldirqIdentityId
    , ldirqMaxResults
    , ldirqIdentityPoolId

    -- * Response
    , LookupDeveloperIdentityResponse
    -- ** Response constructor
    , lookupDeveloperIdentityResponse
    -- ** Response lenses
    , ldirsNextToken
    , ldirsIdentityId
    , ldirsDeveloperUserIdentifierList
    , ldirsStatus
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
-- * 'ldirqDeveloperUserIdentifier'
--
-- * 'ldirqNextToken'
--
-- * 'ldirqIdentityId'
--
-- * 'ldirqMaxResults'
--
-- * 'ldirqIdentityPoolId'
data LookupDeveloperIdentity = LookupDeveloperIdentity'
    { _ldirqDeveloperUserIdentifier :: !(Maybe Text)
    , _ldirqNextToken               :: !(Maybe Text)
    , _ldirqIdentityId              :: !(Maybe Text)
    , _ldirqMaxResults              :: !(Maybe Nat)
    , _ldirqIdentityPoolId          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'LookupDeveloperIdentity' smart constructor.
lookupDeveloperIdentity :: Text -> LookupDeveloperIdentity
lookupDeveloperIdentity pIdentityPoolId =
    LookupDeveloperIdentity'
    { _ldirqDeveloperUserIdentifier = Nothing
    , _ldirqNextToken = Nothing
    , _ldirqIdentityId = Nothing
    , _ldirqMaxResults = Nothing
    , _ldirqIdentityPoolId = pIdentityPoolId
    }

-- | A unique ID used by your backend authentication process to identify a
-- user. Typically, a developer identity provider would issue many
-- developer user identifiers, in keeping with the number of users.
ldirqDeveloperUserIdentifier :: Lens' LookupDeveloperIdentity (Maybe Text)
ldirqDeveloperUserIdentifier = lens _ldirqDeveloperUserIdentifier (\ s a -> s{_ldirqDeveloperUserIdentifier = a});

-- | A pagination token. The first call you make will have @NextToken@ set to
-- null. After that the service will return @NextToken@ values as needed.
-- For example, let\'s say you make a request with @MaxResults@ set to 10,
-- and there are 20 matches in the database. The service will return a
-- pagination token as a part of the response. This token can be used to
-- call the API again and get results starting from the 11th match.
ldirqNextToken :: Lens' LookupDeveloperIdentity (Maybe Text)
ldirqNextToken = lens _ldirqNextToken (\ s a -> s{_ldirqNextToken = a});

-- | A unique identifier in the format REGION:GUID.
ldirqIdentityId :: Lens' LookupDeveloperIdentity (Maybe Text)
ldirqIdentityId = lens _ldirqIdentityId (\ s a -> s{_ldirqIdentityId = a});

-- | The maximum number of identities to return.
ldirqMaxResults :: Lens' LookupDeveloperIdentity (Maybe Natural)
ldirqMaxResults = lens _ldirqMaxResults (\ s a -> s{_ldirqMaxResults = a}) . mapping _Nat;

-- | An identity pool ID in the format REGION:GUID.
ldirqIdentityPoolId :: Lens' LookupDeveloperIdentity Text
ldirqIdentityPoolId = lens _ldirqIdentityPoolId (\ s a -> s{_ldirqIdentityPoolId = a});

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
                 _ldirqDeveloperUserIdentifier,
               "NextToken" .= _ldirqNextToken,
               "IdentityId" .= _ldirqIdentityId,
               "MaxResults" .= _ldirqMaxResults,
               "IdentityPoolId" .= _ldirqIdentityPoolId]

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
-- * 'ldirsNextToken'
--
-- * 'ldirsIdentityId'
--
-- * 'ldirsDeveloperUserIdentifierList'
--
-- * 'ldirsStatus'
data LookupDeveloperIdentityResponse = LookupDeveloperIdentityResponse'
    { _ldirsNextToken                   :: !(Maybe Text)
    , _ldirsIdentityId                  :: !(Maybe Text)
    , _ldirsDeveloperUserIdentifierList :: !(Maybe [Text])
    , _ldirsStatus                      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'LookupDeveloperIdentityResponse' smart constructor.
lookupDeveloperIdentityResponse :: Int -> LookupDeveloperIdentityResponse
lookupDeveloperIdentityResponse pStatus =
    LookupDeveloperIdentityResponse'
    { _ldirsNextToken = Nothing
    , _ldirsIdentityId = Nothing
    , _ldirsDeveloperUserIdentifierList = Nothing
    , _ldirsStatus = pStatus
    }

-- | A pagination token. The first call you make will have @NextToken@ set to
-- null. After that the service will return @NextToken@ values as needed.
-- For example, let\'s say you make a request with @MaxResults@ set to 10,
-- and there are 20 matches in the database. The service will return a
-- pagination token as a part of the response. This token can be used to
-- call the API again and get results starting from the 11th match.
ldirsNextToken :: Lens' LookupDeveloperIdentityResponse (Maybe Text)
ldirsNextToken = lens _ldirsNextToken (\ s a -> s{_ldirsNextToken = a});

-- | A unique identifier in the format REGION:GUID.
ldirsIdentityId :: Lens' LookupDeveloperIdentityResponse (Maybe Text)
ldirsIdentityId = lens _ldirsIdentityId (\ s a -> s{_ldirsIdentityId = a});

-- | This is the list of developer user identifiers associated with an
-- identity ID. Cognito supports the association of multiple developer user
-- identifiers with an identity ID.
ldirsDeveloperUserIdentifierList :: Lens' LookupDeveloperIdentityResponse [Text]
ldirsDeveloperUserIdentifierList = lens _ldirsDeveloperUserIdentifierList (\ s a -> s{_ldirsDeveloperUserIdentifierList = a}) . _Default;

-- | FIXME: Undocumented member.
ldirsStatus :: Lens' LookupDeveloperIdentityResponse Int
ldirsStatus = lens _ldirsStatus (\ s a -> s{_ldirsStatus = a});
