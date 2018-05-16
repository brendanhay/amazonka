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
-- Module      : Network.AWS.MQ.ListConfigurations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all configurations.
module Network.AWS.MQ.ListConfigurations
    (
    -- * Creating a Request
      listConfigurations
    , ListConfigurations
    -- * Request Lenses
    , lcNextToken
    , lcMaxResults

    -- * Destructuring the Response
    , listConfigurationsResponse
    , ListConfigurationsResponse
    -- * Response Lenses
    , lcrsConfigurations
    , lcrsNextToken
    , lcrsMaxResults
    , lcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MQ.Types
import Network.AWS.MQ.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listConfigurations' smart constructor.
data ListConfigurations = ListConfigurations'
  { _lcNextToken  :: !(Maybe Text)
  , _lcMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListConfigurations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcNextToken' - The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
--
-- * 'lcMaxResults' - The maximum number of configurations that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
listConfigurations
    :: ListConfigurations
listConfigurations =
  ListConfigurations' {_lcNextToken = Nothing, _lcMaxResults = Nothing}


-- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
lcNextToken :: Lens' ListConfigurations (Maybe Text)
lcNextToken = lens _lcNextToken (\ s a -> s{_lcNextToken = a})

-- | The maximum number of configurations that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
lcMaxResults :: Lens' ListConfigurations (Maybe Natural)
lcMaxResults = lens _lcMaxResults (\ s a -> s{_lcMaxResults = a}) . mapping _Nat

instance AWSRequest ListConfigurations where
        type Rs ListConfigurations =
             ListConfigurationsResponse
        request = get mq
        response
          = receiveJSON
              (\ s h x ->
                 ListConfigurationsResponse' <$>
                   (x .?> "configurations" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (x .?> "maxResults")
                     <*> (pure (fromEnum s)))

instance Hashable ListConfigurations where

instance NFData ListConfigurations where

instance ToHeaders ListConfigurations where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListConfigurations where
        toPath = const "/v1/configurations"

instance ToQuery ListConfigurations where
        toQuery ListConfigurations'{..}
          = mconcat
              ["nextToken" =: _lcNextToken,
               "maxResults" =: _lcMaxResults]

-- | /See:/ 'listConfigurationsResponse' smart constructor.
data ListConfigurationsResponse = ListConfigurationsResponse'
  { _lcrsConfigurations :: !(Maybe [Configuration])
  , _lcrsNextToken      :: !(Maybe Text)
  , _lcrsMaxResults     :: !(Maybe Int)
  , _lcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListConfigurationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcrsConfigurations' - The list of all revisions for the specified configuration.
--
-- * 'lcrsNextToken' - The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
--
-- * 'lcrsMaxResults' - The maximum number of configurations that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
--
-- * 'lcrsResponseStatus' - -- | The response status code.
listConfigurationsResponse
    :: Int -- ^ 'lcrsResponseStatus'
    -> ListConfigurationsResponse
listConfigurationsResponse pResponseStatus_ =
  ListConfigurationsResponse'
    { _lcrsConfigurations = Nothing
    , _lcrsNextToken = Nothing
    , _lcrsMaxResults = Nothing
    , _lcrsResponseStatus = pResponseStatus_
    }


-- | The list of all revisions for the specified configuration.
lcrsConfigurations :: Lens' ListConfigurationsResponse [Configuration]
lcrsConfigurations = lens _lcrsConfigurations (\ s a -> s{_lcrsConfigurations = a}) . _Default . _Coerce

-- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
lcrsNextToken :: Lens' ListConfigurationsResponse (Maybe Text)
lcrsNextToken = lens _lcrsNextToken (\ s a -> s{_lcrsNextToken = a})

-- | The maximum number of configurations that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
lcrsMaxResults :: Lens' ListConfigurationsResponse (Maybe Int)
lcrsMaxResults = lens _lcrsMaxResults (\ s a -> s{_lcrsMaxResults = a})

-- | -- | The response status code.
lcrsResponseStatus :: Lens' ListConfigurationsResponse Int
lcrsResponseStatus = lens _lcrsResponseStatus (\ s a -> s{_lcrsResponseStatus = a})

instance NFData ListConfigurationsResponse where
