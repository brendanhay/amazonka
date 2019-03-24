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
-- Module      : Network.AWS.AlexaBusiness.ListConferenceProviders
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists conference providers under a specific AWS account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.ListConferenceProviders
    (
    -- * Creating a Request
      listConferenceProviders
    , ListConferenceProviders
    -- * Request Lenses
    , lcpNextToken
    , lcpMaxResults

    -- * Destructuring the Response
    , listConferenceProvidersResponse
    , ListConferenceProvidersResponse
    -- * Response Lenses
    , lcprsConferenceProviders
    , lcprsNextToken
    , lcprsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listConferenceProviders' smart constructor.
data ListConferenceProviders = ListConferenceProviders'
  { _lcpNextToken  :: !(Maybe Text)
  , _lcpMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListConferenceProviders' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcpNextToken' - The tokens used for pagination.
--
-- * 'lcpMaxResults' - The maximum number of conference providers to be returned, per paginated calls.
listConferenceProviders
    :: ListConferenceProviders
listConferenceProviders =
  ListConferenceProviders' {_lcpNextToken = Nothing, _lcpMaxResults = Nothing}


-- | The tokens used for pagination.
lcpNextToken :: Lens' ListConferenceProviders (Maybe Text)
lcpNextToken = lens _lcpNextToken (\ s a -> s{_lcpNextToken = a})

-- | The maximum number of conference providers to be returned, per paginated calls.
lcpMaxResults :: Lens' ListConferenceProviders (Maybe Natural)
lcpMaxResults = lens _lcpMaxResults (\ s a -> s{_lcpMaxResults = a}) . mapping _Nat

instance AWSPager ListConferenceProviders where
        page rq rs
          | stop (rs ^. lcprsNextToken) = Nothing
          | stop (rs ^. lcprsConferenceProviders) = Nothing
          | otherwise =
            Just $ rq & lcpNextToken .~ rs ^. lcprsNextToken

instance AWSRequest ListConferenceProviders where
        type Rs ListConferenceProviders =
             ListConferenceProvidersResponse
        request = postJSON alexaBusiness
        response
          = receiveJSON
              (\ s h x ->
                 ListConferenceProvidersResponse' <$>
                   (x .?> "ConferenceProviders" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListConferenceProviders where

instance NFData ListConferenceProviders where

instance ToHeaders ListConferenceProviders where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.ListConferenceProviders" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListConferenceProviders where
        toJSON ListConferenceProviders'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lcpNextToken,
                  ("MaxResults" .=) <$> _lcpMaxResults])

instance ToPath ListConferenceProviders where
        toPath = const "/"

instance ToQuery ListConferenceProviders where
        toQuery = const mempty

-- | /See:/ 'listConferenceProvidersResponse' smart constructor.
data ListConferenceProvidersResponse = ListConferenceProvidersResponse'
  { _lcprsConferenceProviders :: !(Maybe [ConferenceProvider])
  , _lcprsNextToken           :: !(Maybe Text)
  , _lcprsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListConferenceProvidersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcprsConferenceProviders' - The conference providers.
--
-- * 'lcprsNextToken' - The tokens used for pagination.
--
-- * 'lcprsResponseStatus' - -- | The response status code.
listConferenceProvidersResponse
    :: Int -- ^ 'lcprsResponseStatus'
    -> ListConferenceProvidersResponse
listConferenceProvidersResponse pResponseStatus_ =
  ListConferenceProvidersResponse'
    { _lcprsConferenceProviders = Nothing
    , _lcprsNextToken = Nothing
    , _lcprsResponseStatus = pResponseStatus_
    }


-- | The conference providers.
lcprsConferenceProviders :: Lens' ListConferenceProvidersResponse [ConferenceProvider]
lcprsConferenceProviders = lens _lcprsConferenceProviders (\ s a -> s{_lcprsConferenceProviders = a}) . _Default . _Coerce

-- | The tokens used for pagination.
lcprsNextToken :: Lens' ListConferenceProvidersResponse (Maybe Text)
lcprsNextToken = lens _lcprsNextToken (\ s a -> s{_lcprsNextToken = a})

-- | -- | The response status code.
lcprsResponseStatus :: Lens' ListConferenceProvidersResponse Int
lcprsResponseStatus = lens _lcprsResponseStatus (\ s a -> s{_lcprsResponseStatus = a})

instance NFData ListConferenceProvidersResponse where
