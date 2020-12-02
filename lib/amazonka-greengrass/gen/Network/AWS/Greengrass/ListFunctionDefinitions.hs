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
-- Module      : Network.AWS.Greengrass.ListFunctionDefinitions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of Lambda function definitions.
module Network.AWS.Greengrass.ListFunctionDefinitions
    (
    -- * Creating a Request
      listFunctionDefinitions
    , ListFunctionDefinitions
    -- * Request Lenses
    , lfdNextToken
    , lfdMaxResults

    -- * Destructuring the Response
    , listFunctionDefinitionsResponse
    , ListFunctionDefinitionsResponse
    -- * Response Lenses
    , lfdrsNextToken
    , lfdrsDefinitions
    , lfdrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listFunctionDefinitions' smart constructor.
data ListFunctionDefinitions = ListFunctionDefinitions'
  { _lfdNextToken  :: !(Maybe Text)
  , _lfdMaxResults :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListFunctionDefinitions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfdNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'lfdMaxResults' - The maximum number of results to be returned per request.
listFunctionDefinitions
    :: ListFunctionDefinitions
listFunctionDefinitions =
  ListFunctionDefinitions' {_lfdNextToken = Nothing, _lfdMaxResults = Nothing}


-- | The token for the next set of results, or ''null'' if there are no additional results.
lfdNextToken :: Lens' ListFunctionDefinitions (Maybe Text)
lfdNextToken = lens _lfdNextToken (\ s a -> s{_lfdNextToken = a})

-- | The maximum number of results to be returned per request.
lfdMaxResults :: Lens' ListFunctionDefinitions (Maybe Text)
lfdMaxResults = lens _lfdMaxResults (\ s a -> s{_lfdMaxResults = a})

instance AWSRequest ListFunctionDefinitions where
        type Rs ListFunctionDefinitions =
             ListFunctionDefinitionsResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 ListFunctionDefinitionsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "Definitions" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListFunctionDefinitions where

instance NFData ListFunctionDefinitions where

instance ToHeaders ListFunctionDefinitions where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListFunctionDefinitions where
        toPath = const "/greengrass/definition/functions"

instance ToQuery ListFunctionDefinitions where
        toQuery ListFunctionDefinitions'{..}
          = mconcat
              ["NextToken" =: _lfdNextToken,
               "MaxResults" =: _lfdMaxResults]

-- | /See:/ 'listFunctionDefinitionsResponse' smart constructor.
data ListFunctionDefinitionsResponse = ListFunctionDefinitionsResponse'
  { _lfdrsNextToken      :: !(Maybe Text)
  , _lfdrsDefinitions    :: !(Maybe [DefinitionInformation])
  , _lfdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListFunctionDefinitionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfdrsNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'lfdrsDefinitions' - Information about a definition.
--
-- * 'lfdrsResponseStatus' - -- | The response status code.
listFunctionDefinitionsResponse
    :: Int -- ^ 'lfdrsResponseStatus'
    -> ListFunctionDefinitionsResponse
listFunctionDefinitionsResponse pResponseStatus_ =
  ListFunctionDefinitionsResponse'
    { _lfdrsNextToken = Nothing
    , _lfdrsDefinitions = Nothing
    , _lfdrsResponseStatus = pResponseStatus_
    }


-- | The token for the next set of results, or ''null'' if there are no additional results.
lfdrsNextToken :: Lens' ListFunctionDefinitionsResponse (Maybe Text)
lfdrsNextToken = lens _lfdrsNextToken (\ s a -> s{_lfdrsNextToken = a})

-- | Information about a definition.
lfdrsDefinitions :: Lens' ListFunctionDefinitionsResponse [DefinitionInformation]
lfdrsDefinitions = lens _lfdrsDefinitions (\ s a -> s{_lfdrsDefinitions = a}) . _Default . _Coerce

-- | -- | The response status code.
lfdrsResponseStatus :: Lens' ListFunctionDefinitionsResponse Int
lfdrsResponseStatus = lens _lfdrsResponseStatus (\ s a -> s{_lfdrsResponseStatus = a})

instance NFData ListFunctionDefinitionsResponse where
