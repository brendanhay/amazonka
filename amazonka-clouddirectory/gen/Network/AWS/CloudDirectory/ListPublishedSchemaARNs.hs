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
-- Module      : Network.AWS.CloudDirectory.ListPublishedSchemaARNs
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves each published schema Amazon Resource Name (ARN).
--
--
module Network.AWS.CloudDirectory.ListPublishedSchemaARNs
    (
    -- * Creating a Request
      listPublishedSchemaARNs
    , ListPublishedSchemaARNs
    -- * Request Lenses
    , lpsaNextToken
    , lpsaMaxResults

    -- * Destructuring the Response
    , listPublishedSchemaARNsResponse
    , ListPublishedSchemaARNsResponse
    -- * Response Lenses
    , lpsarsSchemaARNs
    , lpsarsNextToken
    , lpsarsResponseStatus
    ) where

import           Network.AWS.CloudDirectory.Types
import           Network.AWS.CloudDirectory.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listPublishedSchemaARNs' smart constructor.
data ListPublishedSchemaARNs = ListPublishedSchemaARNs'
    { _lpsaNextToken  :: !(Maybe Text)
    , _lpsaMaxResults :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListPublishedSchemaARNs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpsaNextToken' - The pagination token.
--
-- * 'lpsaMaxResults' - The maximum number of results to retrieve.
listPublishedSchemaARNs
    :: ListPublishedSchemaARNs
listPublishedSchemaARNs =
    ListPublishedSchemaARNs'
    { _lpsaNextToken = Nothing
    , _lpsaMaxResults = Nothing
    }

-- | The pagination token.
lpsaNextToken :: Lens' ListPublishedSchemaARNs (Maybe Text)
lpsaNextToken = lens _lpsaNextToken (\ s a -> s{_lpsaNextToken = a});

-- | The maximum number of results to retrieve.
lpsaMaxResults :: Lens' ListPublishedSchemaARNs (Maybe Natural)
lpsaMaxResults = lens _lpsaMaxResults (\ s a -> s{_lpsaMaxResults = a}) . mapping _Nat;

instance AWSRequest ListPublishedSchemaARNs where
        type Rs ListPublishedSchemaARNs =
             ListPublishedSchemaARNsResponse
        request = postJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 ListPublishedSchemaARNsResponse' <$>
                   (x .?> "SchemaArns" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListPublishedSchemaARNs

instance NFData ListPublishedSchemaARNs

instance ToHeaders ListPublishedSchemaARNs where
        toHeaders = const mempty

instance ToJSON ListPublishedSchemaARNs where
        toJSON ListPublishedSchemaARNs'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lpsaNextToken,
                  ("MaxResults" .=) <$> _lpsaMaxResults])

instance ToPath ListPublishedSchemaARNs where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/schema/published"

instance ToQuery ListPublishedSchemaARNs where
        toQuery = const mempty

-- | /See:/ 'listPublishedSchemaARNsResponse' smart constructor.
data ListPublishedSchemaARNsResponse = ListPublishedSchemaARNsResponse'
    { _lpsarsSchemaARNs     :: !(Maybe [Text])
    , _lpsarsNextToken      :: !(Maybe Text)
    , _lpsarsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListPublishedSchemaARNsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpsarsSchemaARNs' - The ARNs of published schemas.
--
-- * 'lpsarsNextToken' - The pagination token.
--
-- * 'lpsarsResponseStatus' - -- | The response status code.
listPublishedSchemaARNsResponse
    :: Int -- ^ 'lpsarsResponseStatus'
    -> ListPublishedSchemaARNsResponse
listPublishedSchemaARNsResponse pResponseStatus_ =
    ListPublishedSchemaARNsResponse'
    { _lpsarsSchemaARNs = Nothing
    , _lpsarsNextToken = Nothing
    , _lpsarsResponseStatus = pResponseStatus_
    }

-- | The ARNs of published schemas.
lpsarsSchemaARNs :: Lens' ListPublishedSchemaARNsResponse [Text]
lpsarsSchemaARNs = lens _lpsarsSchemaARNs (\ s a -> s{_lpsarsSchemaARNs = a}) . _Default . _Coerce;

-- | The pagination token.
lpsarsNextToken :: Lens' ListPublishedSchemaARNsResponse (Maybe Text)
lpsarsNextToken = lens _lpsarsNextToken (\ s a -> s{_lpsarsNextToken = a});

-- | -- | The response status code.
lpsarsResponseStatus :: Lens' ListPublishedSchemaARNsResponse Int
lpsarsResponseStatus = lens _lpsarsResponseStatus (\ s a -> s{_lpsarsResponseStatus = a});

instance NFData ListPublishedSchemaARNsResponse
