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
-- Module      : Network.AWS.MQ.ListConfigurationRevisions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all revisions for the specified configuration.
module Network.AWS.MQ.ListConfigurationRevisions
    (
    -- * Creating a Request
      listConfigurationRevisions
    , ListConfigurationRevisions
    -- * Request Lenses
    , lcrNextToken
    , lcrMaxResults
    , lcrConfigurationId

    -- * Destructuring the Response
    , listConfigurationRevisionsResponse
    , ListConfigurationRevisionsResponse
    -- * Response Lenses
    , lcrrsConfigurationId
    , lcrrsNextToken
    , lcrrsRevisions
    , lcrrsMaxResults
    , lcrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MQ.Types
import Network.AWS.MQ.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listConfigurationRevisions' smart constructor.
data ListConfigurationRevisions = ListConfigurationRevisions'
  { _lcrNextToken       :: !(Maybe Text)
  , _lcrMaxResults      :: !(Maybe Nat)
  , _lcrConfigurationId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListConfigurationRevisions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcrNextToken' - The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
--
-- * 'lcrMaxResults' - The maximum number of configurations that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
--
-- * 'lcrConfigurationId' - The unique ID that Amazon MQ generates for the configuration.
listConfigurationRevisions
    :: Text -- ^ 'lcrConfigurationId'
    -> ListConfigurationRevisions
listConfigurationRevisions pConfigurationId_ =
  ListConfigurationRevisions'
    { _lcrNextToken = Nothing
    , _lcrMaxResults = Nothing
    , _lcrConfigurationId = pConfigurationId_
    }


-- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
lcrNextToken :: Lens' ListConfigurationRevisions (Maybe Text)
lcrNextToken = lens _lcrNextToken (\ s a -> s{_lcrNextToken = a})

-- | The maximum number of configurations that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
lcrMaxResults :: Lens' ListConfigurationRevisions (Maybe Natural)
lcrMaxResults = lens _lcrMaxResults (\ s a -> s{_lcrMaxResults = a}) . mapping _Nat

-- | The unique ID that Amazon MQ generates for the configuration.
lcrConfigurationId :: Lens' ListConfigurationRevisions Text
lcrConfigurationId = lens _lcrConfigurationId (\ s a -> s{_lcrConfigurationId = a})

instance AWSRequest ListConfigurationRevisions where
        type Rs ListConfigurationRevisions =
             ListConfigurationRevisionsResponse
        request = get mq
        response
          = receiveJSON
              (\ s h x ->
                 ListConfigurationRevisionsResponse' <$>
                   (x .?> "configurationId") <*> (x .?> "nextToken") <*>
                     (x .?> "revisions" .!@ mempty)
                     <*> (x .?> "maxResults")
                     <*> (pure (fromEnum s)))

instance Hashable ListConfigurationRevisions where

instance NFData ListConfigurationRevisions where

instance ToHeaders ListConfigurationRevisions where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListConfigurationRevisions where
        toPath ListConfigurationRevisions'{..}
          = mconcat
              ["/v1/configurations/", toBS _lcrConfigurationId,
               "/revisions"]

instance ToQuery ListConfigurationRevisions where
        toQuery ListConfigurationRevisions'{..}
          = mconcat
              ["nextToken" =: _lcrNextToken,
               "maxResults" =: _lcrMaxResults]

-- | /See:/ 'listConfigurationRevisionsResponse' smart constructor.
data ListConfigurationRevisionsResponse = ListConfigurationRevisionsResponse'
  { _lcrrsConfigurationId :: !(Maybe Text)
  , _lcrrsNextToken       :: !(Maybe Text)
  , _lcrrsRevisions       :: !(Maybe [ConfigurationRevision])
  , _lcrrsMaxResults      :: !(Maybe Int)
  , _lcrrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListConfigurationRevisionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcrrsConfigurationId' - The unique ID that Amazon MQ generates for the configuration.
--
-- * 'lcrrsNextToken' - The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
--
-- * 'lcrrsRevisions' - The list of all revisions for the specified configuration.
--
-- * 'lcrrsMaxResults' - The maximum number of configuration revisions that can be returned per page (20 by default). This value must be an integer from 5 to 100.
--
-- * 'lcrrsResponseStatus' - -- | The response status code.
listConfigurationRevisionsResponse
    :: Int -- ^ 'lcrrsResponseStatus'
    -> ListConfigurationRevisionsResponse
listConfigurationRevisionsResponse pResponseStatus_ =
  ListConfigurationRevisionsResponse'
    { _lcrrsConfigurationId = Nothing
    , _lcrrsNextToken = Nothing
    , _lcrrsRevisions = Nothing
    , _lcrrsMaxResults = Nothing
    , _lcrrsResponseStatus = pResponseStatus_
    }


-- | The unique ID that Amazon MQ generates for the configuration.
lcrrsConfigurationId :: Lens' ListConfigurationRevisionsResponse (Maybe Text)
lcrrsConfigurationId = lens _lcrrsConfigurationId (\ s a -> s{_lcrrsConfigurationId = a})

-- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
lcrrsNextToken :: Lens' ListConfigurationRevisionsResponse (Maybe Text)
lcrrsNextToken = lens _lcrrsNextToken (\ s a -> s{_lcrrsNextToken = a})

-- | The list of all revisions for the specified configuration.
lcrrsRevisions :: Lens' ListConfigurationRevisionsResponse [ConfigurationRevision]
lcrrsRevisions = lens _lcrrsRevisions (\ s a -> s{_lcrrsRevisions = a}) . _Default . _Coerce

-- | The maximum number of configuration revisions that can be returned per page (20 by default). This value must be an integer from 5 to 100.
lcrrsMaxResults :: Lens' ListConfigurationRevisionsResponse (Maybe Int)
lcrrsMaxResults = lens _lcrrsMaxResults (\ s a -> s{_lcrrsMaxResults = a})

-- | -- | The response status code.
lcrrsResponseStatus :: Lens' ListConfigurationRevisionsResponse Int
lcrrsResponseStatus = lens _lcrrsResponseStatus (\ s a -> s{_lcrrsResponseStatus = a})

instance NFData ListConfigurationRevisionsResponse
         where
