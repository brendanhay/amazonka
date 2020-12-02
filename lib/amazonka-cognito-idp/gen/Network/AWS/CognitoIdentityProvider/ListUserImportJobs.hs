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
-- Module      : Network.AWS.CognitoIdentityProvider.ListUserImportJobs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the user import jobs.
--
--
module Network.AWS.CognitoIdentityProvider.ListUserImportJobs
    (
    -- * Creating a Request
      listUserImportJobs
    , ListUserImportJobs
    -- * Request Lenses
    , luijPaginationToken
    , luijUserPoolId
    , luijMaxResults

    -- * Destructuring the Response
    , listUserImportJobsResponse
    , ListUserImportJobsResponse
    -- * Response Lenses
    , luijrsPaginationToken
    , luijrsUserImportJobs
    , luijrsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to list the user import jobs.
--
--
--
-- /See:/ 'listUserImportJobs' smart constructor.
data ListUserImportJobs = ListUserImportJobs'
  { _luijPaginationToken :: !(Maybe Text)
  , _luijUserPoolId      :: !Text
  , _luijMaxResults      :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListUserImportJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'luijPaginationToken' - An identifier that was returned from the previous call to @ListUserImportJobs@ , which can be used to return the next set of import jobs in the list.
--
-- * 'luijUserPoolId' - The user pool ID for the user pool that the users are being imported into.
--
-- * 'luijMaxResults' - The maximum number of import jobs you want the request to return.
listUserImportJobs
    :: Text -- ^ 'luijUserPoolId'
    -> Natural -- ^ 'luijMaxResults'
    -> ListUserImportJobs
listUserImportJobs pUserPoolId_ pMaxResults_ =
  ListUserImportJobs'
    { _luijPaginationToken = Nothing
    , _luijUserPoolId = pUserPoolId_
    , _luijMaxResults = _Nat # pMaxResults_
    }


-- | An identifier that was returned from the previous call to @ListUserImportJobs@ , which can be used to return the next set of import jobs in the list.
luijPaginationToken :: Lens' ListUserImportJobs (Maybe Text)
luijPaginationToken = lens _luijPaginationToken (\ s a -> s{_luijPaginationToken = a})

-- | The user pool ID for the user pool that the users are being imported into.
luijUserPoolId :: Lens' ListUserImportJobs Text
luijUserPoolId = lens _luijUserPoolId (\ s a -> s{_luijUserPoolId = a})

-- | The maximum number of import jobs you want the request to return.
luijMaxResults :: Lens' ListUserImportJobs Natural
luijMaxResults = lens _luijMaxResults (\ s a -> s{_luijMaxResults = a}) . _Nat

instance AWSRequest ListUserImportJobs where
        type Rs ListUserImportJobs =
             ListUserImportJobsResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 ListUserImportJobsResponse' <$>
                   (x .?> "PaginationToken") <*>
                     (x .?> "UserImportJobs")
                     <*> (pure (fromEnum s)))

instance Hashable ListUserImportJobs where

instance NFData ListUserImportJobs where

instance ToHeaders ListUserImportJobs where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.ListUserImportJobs"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListUserImportJobs where
        toJSON ListUserImportJobs'{..}
          = object
              (catMaybes
                 [("PaginationToken" .=) <$> _luijPaginationToken,
                  Just ("UserPoolId" .= _luijUserPoolId),
                  Just ("MaxResults" .= _luijMaxResults)])

instance ToPath ListUserImportJobs where
        toPath = const "/"

instance ToQuery ListUserImportJobs where
        toQuery = const mempty

-- | Represents the response from the server to the request to list the user import jobs.
--
--
--
-- /See:/ 'listUserImportJobsResponse' smart constructor.
data ListUserImportJobsResponse = ListUserImportJobsResponse'
  { _luijrsPaginationToken :: !(Maybe Text)
  , _luijrsUserImportJobs  :: !(Maybe (List1 UserImportJobType))
  , _luijrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListUserImportJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'luijrsPaginationToken' - An identifier that can be used to return the next set of user import jobs in the list.
--
-- * 'luijrsUserImportJobs' - The user import jobs.
--
-- * 'luijrsResponseStatus' - -- | The response status code.
listUserImportJobsResponse
    :: Int -- ^ 'luijrsResponseStatus'
    -> ListUserImportJobsResponse
listUserImportJobsResponse pResponseStatus_ =
  ListUserImportJobsResponse'
    { _luijrsPaginationToken = Nothing
    , _luijrsUserImportJobs = Nothing
    , _luijrsResponseStatus = pResponseStatus_
    }


-- | An identifier that can be used to return the next set of user import jobs in the list.
luijrsPaginationToken :: Lens' ListUserImportJobsResponse (Maybe Text)
luijrsPaginationToken = lens _luijrsPaginationToken (\ s a -> s{_luijrsPaginationToken = a})

-- | The user import jobs.
luijrsUserImportJobs :: Lens' ListUserImportJobsResponse (Maybe (NonEmpty UserImportJobType))
luijrsUserImportJobs = lens _luijrsUserImportJobs (\ s a -> s{_luijrsUserImportJobs = a}) . mapping _List1

-- | -- | The response status code.
luijrsResponseStatus :: Lens' ListUserImportJobsResponse Int
luijrsResponseStatus = lens _luijrsResponseStatus (\ s a -> s{_luijrsResponseStatus = a})

instance NFData ListUserImportJobsResponse where
