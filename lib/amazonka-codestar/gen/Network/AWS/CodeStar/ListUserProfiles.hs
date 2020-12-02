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
-- Module      : Network.AWS.CodeStar.ListUserProfiles
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the user profiles configured for your AWS account in AWS CodeStar.
--
--
module Network.AWS.CodeStar.ListUserProfiles
    (
    -- * Creating a Request
      listUserProfiles
    , ListUserProfiles
    -- * Request Lenses
    , lupNextToken
    , lupMaxResults

    -- * Destructuring the Response
    , listUserProfilesResponse
    , ListUserProfilesResponse
    -- * Response Lenses
    , luprsNextToken
    , luprsResponseStatus
    , luprsUserProfiles
    ) where

import Network.AWS.CodeStar.Types
import Network.AWS.CodeStar.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listUserProfiles' smart constructor.
data ListUserProfiles = ListUserProfiles'
  { _lupNextToken  :: !(Maybe Text)
  , _lupMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListUserProfiles' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lupNextToken' - The continuation token for the next set of results, if the results cannot be returned in one response.
--
-- * 'lupMaxResults' - The maximum number of results to return in a response.
listUserProfiles
    :: ListUserProfiles
listUserProfiles =
  ListUserProfiles' {_lupNextToken = Nothing, _lupMaxResults = Nothing}


-- | The continuation token for the next set of results, if the results cannot be returned in one response.
lupNextToken :: Lens' ListUserProfiles (Maybe Text)
lupNextToken = lens _lupNextToken (\ s a -> s{_lupNextToken = a})

-- | The maximum number of results to return in a response.
lupMaxResults :: Lens' ListUserProfiles (Maybe Natural)
lupMaxResults = lens _lupMaxResults (\ s a -> s{_lupMaxResults = a}) . mapping _Nat

instance AWSRequest ListUserProfiles where
        type Rs ListUserProfiles = ListUserProfilesResponse
        request = postJSON codeStar
        response
          = receiveJSON
              (\ s h x ->
                 ListUserProfilesResponse' <$>
                   (x .?> "nextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "userProfiles" .!@ mempty))

instance Hashable ListUserProfiles where

instance NFData ListUserProfiles where

instance ToHeaders ListUserProfiles where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeStar_20170419.ListUserProfiles" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListUserProfiles where
        toJSON ListUserProfiles'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _lupNextToken,
                  ("maxResults" .=) <$> _lupMaxResults])

instance ToPath ListUserProfiles where
        toPath = const "/"

instance ToQuery ListUserProfiles where
        toQuery = const mempty

-- | /See:/ 'listUserProfilesResponse' smart constructor.
data ListUserProfilesResponse = ListUserProfilesResponse'
  { _luprsNextToken      :: !(Maybe Text)
  , _luprsResponseStatus :: !Int
  , _luprsUserProfiles   :: ![UserProfileSummary]
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListUserProfilesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'luprsNextToken' - The continuation token to use when requesting the next set of results, if there are more results to be returned.
--
-- * 'luprsResponseStatus' - -- | The response status code.
--
-- * 'luprsUserProfiles' - All the user profiles configured in AWS CodeStar for an AWS account.
listUserProfilesResponse
    :: Int -- ^ 'luprsResponseStatus'
    -> ListUserProfilesResponse
listUserProfilesResponse pResponseStatus_ =
  ListUserProfilesResponse'
    { _luprsNextToken = Nothing
    , _luprsResponseStatus = pResponseStatus_
    , _luprsUserProfiles = mempty
    }


-- | The continuation token to use when requesting the next set of results, if there are more results to be returned.
luprsNextToken :: Lens' ListUserProfilesResponse (Maybe Text)
luprsNextToken = lens _luprsNextToken (\ s a -> s{_luprsNextToken = a})

-- | -- | The response status code.
luprsResponseStatus :: Lens' ListUserProfilesResponse Int
luprsResponseStatus = lens _luprsResponseStatus (\ s a -> s{_luprsResponseStatus = a})

-- | All the user profiles configured in AWS CodeStar for an AWS account.
luprsUserProfiles :: Lens' ListUserProfilesResponse [UserProfileSummary]
luprsUserProfiles = lens _luprsUserProfiles (\ s a -> s{_luprsUserProfiles = a}) . _Coerce

instance NFData ListUserProfilesResponse where
