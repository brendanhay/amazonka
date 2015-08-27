{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.LookupDeveloperIdentity
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the 'IdentityID' associated with a 'DeveloperUserIdentifier'
-- or the list of 'DeveloperUserIdentifier's associated with an
-- 'IdentityId' for an existing identity. Either 'IdentityID' or
-- 'DeveloperUserIdentifier' must not be null. If you supply only one of
-- these values, the other value will be searched in the database and
-- returned as a part of the response. If you supply both,
-- 'DeveloperUserIdentifier' will be matched against 'IdentityID'. If the
-- values are verified against the database, the response returns both
-- values and is the same as the request. Otherwise a
-- 'ResourceConflictException' is thrown.
--
-- You must use AWS Developer credentials to call this API.
--
-- /See:/ <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_LookupDeveloperIdentity.html AWS API Reference> for LookupDeveloperIdentity.
module Network.AWS.CognitoIdentity.LookupDeveloperIdentity
    (
    -- * Creating a Request
      lookupDeveloperIdentity
    , LookupDeveloperIdentity
    -- * Request Lenses
    , ldiDeveloperUserIdentifier
    , ldiNextToken
    , ldiIdentityId
    , ldiMaxResults
    , ldiIdentityPoolId

    -- * Destructuring the Response
    , lookupDeveloperIdentityResponse
    , LookupDeveloperIdentityResponse
    -- * Response Lenses
    , ldirsNextToken
    , ldirsIdentityId
    , ldirsDeveloperUserIdentifierList
    , ldirsStatus
    ) where

import           Network.AWS.CognitoIdentity.Types
import           Network.AWS.CognitoIdentity.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Input to the 'LookupDeveloperIdentityInput' action.
--
-- /See:/ 'lookupDeveloperIdentity' smart constructor.
data LookupDeveloperIdentity = LookupDeveloperIdentity'
    { _ldiDeveloperUserIdentifier :: !(Maybe Text)
    , _ldiNextToken               :: !(Maybe Text)
    , _ldiIdentityId              :: !(Maybe Text)
    , _ldiMaxResults              :: !(Maybe Nat)
    , _ldiIdentityPoolId          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'LookupDeveloperIdentity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
lookupDeveloperIdentity
    :: Text -- ^ 'ldiIdentityPoolId'
    -> LookupDeveloperIdentity
lookupDeveloperIdentity pIdentityPoolId_ =
    LookupDeveloperIdentity'
    { _ldiDeveloperUserIdentifier = Nothing
    , _ldiNextToken = Nothing
    , _ldiIdentityId = Nothing
    , _ldiMaxResults = Nothing
    , _ldiIdentityPoolId = pIdentityPoolId_
    }

-- | A unique ID used by your backend authentication process to identify a
-- user. Typically, a developer identity provider would issue many
-- developer user identifiers, in keeping with the number of users.
ldiDeveloperUserIdentifier :: Lens' LookupDeveloperIdentity (Maybe Text)
ldiDeveloperUserIdentifier = lens _ldiDeveloperUserIdentifier (\ s a -> s{_ldiDeveloperUserIdentifier = a});

-- | A pagination token. The first call you make will have 'NextToken' set to
-- null. After that the service will return 'NextToken' values as needed.
-- For example, let\'s say you make a request with 'MaxResults' set to 10,
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
        type Rs LookupDeveloperIdentity =
             LookupDeveloperIdentityResponse
        request = postJSON cognitoIdentity
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
              (catMaybes
                 [("DeveloperUserIdentifier" .=) <$>
                    _ldiDeveloperUserIdentifier,
                  ("NextToken" .=) <$> _ldiNextToken,
                  ("IdentityId" .=) <$> _ldiIdentityId,
                  ("MaxResults" .=) <$> _ldiMaxResults,
                  Just ("IdentityPoolId" .= _ldiIdentityPoolId)])

instance ToPath LookupDeveloperIdentity where
        toPath = const "/"

instance ToQuery LookupDeveloperIdentity where
        toQuery = const mempty

-- | Returned in response to a successful 'LookupDeveloperIdentity' action.
--
-- /See:/ 'lookupDeveloperIdentityResponse' smart constructor.
data LookupDeveloperIdentityResponse = LookupDeveloperIdentityResponse'
    { _ldirsNextToken                   :: !(Maybe Text)
    , _ldirsIdentityId                  :: !(Maybe Text)
    , _ldirsDeveloperUserIdentifierList :: !(Maybe [Text])
    , _ldirsStatus                      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'LookupDeveloperIdentityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldirsNextToken'
--
-- * 'ldirsIdentityId'
--
-- * 'ldirsDeveloperUserIdentifierList'
--
-- * 'ldirsStatus'
lookupDeveloperIdentityResponse
    :: Int -- ^ 'ldirsStatus'
    -> LookupDeveloperIdentityResponse
lookupDeveloperIdentityResponse pStatus_ =
    LookupDeveloperIdentityResponse'
    { _ldirsNextToken = Nothing
    , _ldirsIdentityId = Nothing
    , _ldirsDeveloperUserIdentifierList = Nothing
    , _ldirsStatus = pStatus_
    }

-- | A pagination token. The first call you make will have 'NextToken' set to
-- null. After that the service will return 'NextToken' values as needed.
-- For example, let\'s say you make a request with 'MaxResults' set to 10,
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
ldirsDeveloperUserIdentifierList = lens _ldirsDeveloperUserIdentifierList (\ s a -> s{_ldirsDeveloperUserIdentifierList = a}) . _Default . _Coerce;

-- | The response status code.
ldirsStatus :: Lens' LookupDeveloperIdentityResponse Int
ldirsStatus = lens _ldirsStatus (\ s a -> s{_ldirsStatus = a});
