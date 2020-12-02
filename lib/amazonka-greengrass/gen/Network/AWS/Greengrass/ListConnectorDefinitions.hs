{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListConnectorDefinitions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of connector definitions.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListConnectorDefinitions
  ( -- * Creating a Request
    listConnectorDefinitions,
    ListConnectorDefinitions,

    -- * Request Lenses
    lNextToken,
    lMaxResults,

    -- * Destructuring the Response
    listConnectorDefinitionsResponse,
    ListConnectorDefinitionsResponse,

    -- * Response Lenses
    lrsNextToken,
    lrsDefinitions,
    lrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listConnectorDefinitions' smart constructor.
data ListConnectorDefinitions = ListConnectorDefinitions'
  { _lNextToken ::
      !(Maybe Text),
    _lMaxResults :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListConnectorDefinitions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'lMaxResults' - The maximum number of results to be returned per request.
listConnectorDefinitions ::
  ListConnectorDefinitions
listConnectorDefinitions =
  ListConnectorDefinitions'
    { _lNextToken = Nothing,
      _lMaxResults = Nothing
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
lNextToken :: Lens' ListConnectorDefinitions (Maybe Text)
lNextToken = lens _lNextToken (\s a -> s {_lNextToken = a})

-- | The maximum number of results to be returned per request.
lMaxResults :: Lens' ListConnectorDefinitions (Maybe Text)
lMaxResults = lens _lMaxResults (\s a -> s {_lMaxResults = a})

instance AWSPager ListConnectorDefinitions where
  page rq rs
    | stop (rs ^. lrsNextToken) = Nothing
    | stop (rs ^. lrsDefinitions) = Nothing
    | otherwise = Just $ rq & lNextToken .~ rs ^. lrsNextToken

instance AWSRequest ListConnectorDefinitions where
  type Rs ListConnectorDefinitions = ListConnectorDefinitionsResponse
  request = get greengrass
  response =
    receiveJSON
      ( \s h x ->
          ListConnectorDefinitionsResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "Definitions" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListConnectorDefinitions

instance NFData ListConnectorDefinitions

instance ToHeaders ListConnectorDefinitions where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath ListConnectorDefinitions where
  toPath = const "/greengrass/definition/connectors"

instance ToQuery ListConnectorDefinitions where
  toQuery ListConnectorDefinitions' {..} =
    mconcat
      ["NextToken" =: _lNextToken, "MaxResults" =: _lMaxResults]

-- | /See:/ 'listConnectorDefinitionsResponse' smart constructor.
data ListConnectorDefinitionsResponse = ListConnectorDefinitionsResponse'
  { _lrsNextToken ::
      !(Maybe Text),
    _lrsDefinitions ::
      !( Maybe
           [DefinitionInformation]
       ),
    _lrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListConnectorDefinitionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrsNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'lrsDefinitions' - Information about a definition.
--
-- * 'lrsResponseStatus' - -- | The response status code.
listConnectorDefinitionsResponse ::
  -- | 'lrsResponseStatus'
  Int ->
  ListConnectorDefinitionsResponse
listConnectorDefinitionsResponse pResponseStatus_ =
  ListConnectorDefinitionsResponse'
    { _lrsNextToken = Nothing,
      _lrsDefinitions = Nothing,
      _lrsResponseStatus = pResponseStatus_
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
lrsNextToken :: Lens' ListConnectorDefinitionsResponse (Maybe Text)
lrsNextToken = lens _lrsNextToken (\s a -> s {_lrsNextToken = a})

-- | Information about a definition.
lrsDefinitions :: Lens' ListConnectorDefinitionsResponse [DefinitionInformation]
lrsDefinitions = lens _lrsDefinitions (\s a -> s {_lrsDefinitions = a}) . _Default . _Coerce

-- | -- | The response status code.
lrsResponseStatus :: Lens' ListConnectorDefinitionsResponse Int
lrsResponseStatus = lens _lrsResponseStatus (\s a -> s {_lrsResponseStatus = a})

instance NFData ListConnectorDefinitionsResponse
