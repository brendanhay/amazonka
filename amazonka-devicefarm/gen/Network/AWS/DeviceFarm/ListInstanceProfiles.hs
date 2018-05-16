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
-- Module      : Network.AWS.DeviceFarm.ListInstanceProfiles
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all the instance profiles in an AWS account.
--
--
module Network.AWS.DeviceFarm.ListInstanceProfiles
    (
    -- * Creating a Request
      listInstanceProfiles
    , ListInstanceProfiles
    -- * Request Lenses
    , lipNextToken
    , lipMaxResults

    -- * Destructuring the Response
    , listInstanceProfilesResponse
    , ListInstanceProfilesResponse
    -- * Response Lenses
    , liprsNextToken
    , liprsInstanceProfiles
    , liprsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listInstanceProfiles' smart constructor.
data ListInstanceProfiles = ListInstanceProfiles'
  { _lipNextToken  :: !(Maybe Text)
  , _lipMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListInstanceProfiles' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lipNextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- * 'lipMaxResults' - An integer specifying the maximum number of items you want to return in the API response.
listInstanceProfiles
    :: ListInstanceProfiles
listInstanceProfiles =
  ListInstanceProfiles' {_lipNextToken = Nothing, _lipMaxResults = Nothing}


-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
lipNextToken :: Lens' ListInstanceProfiles (Maybe Text)
lipNextToken = lens _lipNextToken (\ s a -> s{_lipNextToken = a})

-- | An integer specifying the maximum number of items you want to return in the API response.
lipMaxResults :: Lens' ListInstanceProfiles (Maybe Int)
lipMaxResults = lens _lipMaxResults (\ s a -> s{_lipMaxResults = a})

instance AWSRequest ListInstanceProfiles where
        type Rs ListInstanceProfiles =
             ListInstanceProfilesResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 ListInstanceProfilesResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "instanceProfiles" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListInstanceProfiles where

instance NFData ListInstanceProfiles where

instance ToHeaders ListInstanceProfiles where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.ListInstanceProfiles" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListInstanceProfiles where
        toJSON ListInstanceProfiles'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _lipNextToken,
                  ("maxResults" .=) <$> _lipMaxResults])

instance ToPath ListInstanceProfiles where
        toPath = const "/"

instance ToQuery ListInstanceProfiles where
        toQuery = const mempty

-- | /See:/ 'listInstanceProfilesResponse' smart constructor.
data ListInstanceProfilesResponse = ListInstanceProfilesResponse'
  { _liprsNextToken        :: !(Maybe Text)
  , _liprsInstanceProfiles :: !(Maybe [InstanceProfile])
  , _liprsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListInstanceProfilesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'liprsNextToken' - An identifier that can be used in the next call to this operation to return the next set of items in the list.
--
-- * 'liprsInstanceProfiles' - An object containing information about your instance profiles.
--
-- * 'liprsResponseStatus' - -- | The response status code.
listInstanceProfilesResponse
    :: Int -- ^ 'liprsResponseStatus'
    -> ListInstanceProfilesResponse
listInstanceProfilesResponse pResponseStatus_ =
  ListInstanceProfilesResponse'
    { _liprsNextToken = Nothing
    , _liprsInstanceProfiles = Nothing
    , _liprsResponseStatus = pResponseStatus_
    }


-- | An identifier that can be used in the next call to this operation to return the next set of items in the list.
liprsNextToken :: Lens' ListInstanceProfilesResponse (Maybe Text)
liprsNextToken = lens _liprsNextToken (\ s a -> s{_liprsNextToken = a})

-- | An object containing information about your instance profiles.
liprsInstanceProfiles :: Lens' ListInstanceProfilesResponse [InstanceProfile]
liprsInstanceProfiles = lens _liprsInstanceProfiles (\ s a -> s{_liprsInstanceProfiles = a}) . _Default . _Coerce

-- | -- | The response status code.
liprsResponseStatus :: Lens' ListInstanceProfilesResponse Int
liprsResponseStatus = lens _liprsResponseStatus (\ s a -> s{_liprsResponseStatus = a})

instance NFData ListInstanceProfilesResponse where
