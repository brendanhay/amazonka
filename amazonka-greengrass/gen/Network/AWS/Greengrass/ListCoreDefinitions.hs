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
-- Module      : Network.AWS.Greengrass.ListCoreDefinitions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of core definitions.
module Network.AWS.Greengrass.ListCoreDefinitions
    (
    -- * Creating a Request
      listCoreDefinitions
    , ListCoreDefinitions
    -- * Request Lenses
    , lcdNextToken
    , lcdMaxResults

    -- * Destructuring the Response
    , listCoreDefinitionsResponse
    , ListCoreDefinitionsResponse
    -- * Response Lenses
    , lcdrsNextToken
    , lcdrsDefinitions
    , lcdrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listCoreDefinitions' smart constructor.
data ListCoreDefinitions = ListCoreDefinitions'
  { _lcdNextToken  :: !(Maybe Text)
  , _lcdMaxResults :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListCoreDefinitions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcdNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'lcdMaxResults' - The maximum number of results to be returned per request.
listCoreDefinitions
    :: ListCoreDefinitions
listCoreDefinitions =
  ListCoreDefinitions' {_lcdNextToken = Nothing, _lcdMaxResults = Nothing}


-- | The token for the next set of results, or ''null'' if there are no additional results.
lcdNextToken :: Lens' ListCoreDefinitions (Maybe Text)
lcdNextToken = lens _lcdNextToken (\ s a -> s{_lcdNextToken = a})

-- | The maximum number of results to be returned per request.
lcdMaxResults :: Lens' ListCoreDefinitions (Maybe Text)
lcdMaxResults = lens _lcdMaxResults (\ s a -> s{_lcdMaxResults = a})

instance AWSRequest ListCoreDefinitions where
        type Rs ListCoreDefinitions =
             ListCoreDefinitionsResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 ListCoreDefinitionsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "Definitions" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListCoreDefinitions where

instance NFData ListCoreDefinitions where

instance ToHeaders ListCoreDefinitions where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListCoreDefinitions where
        toPath = const "/greengrass/definition/cores"

instance ToQuery ListCoreDefinitions where
        toQuery ListCoreDefinitions'{..}
          = mconcat
              ["NextToken" =: _lcdNextToken,
               "MaxResults" =: _lcdMaxResults]

-- | /See:/ 'listCoreDefinitionsResponse' smart constructor.
data ListCoreDefinitionsResponse = ListCoreDefinitionsResponse'
  { _lcdrsNextToken      :: !(Maybe Text)
  , _lcdrsDefinitions    :: !(Maybe [DefinitionInformation])
  , _lcdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListCoreDefinitionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcdrsNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'lcdrsDefinitions' - Information about a definition.
--
-- * 'lcdrsResponseStatus' - -- | The response status code.
listCoreDefinitionsResponse
    :: Int -- ^ 'lcdrsResponseStatus'
    -> ListCoreDefinitionsResponse
listCoreDefinitionsResponse pResponseStatus_ =
  ListCoreDefinitionsResponse'
    { _lcdrsNextToken = Nothing
    , _lcdrsDefinitions = Nothing
    , _lcdrsResponseStatus = pResponseStatus_
    }


-- | The token for the next set of results, or ''null'' if there are no additional results.
lcdrsNextToken :: Lens' ListCoreDefinitionsResponse (Maybe Text)
lcdrsNextToken = lens _lcdrsNextToken (\ s a -> s{_lcdrsNextToken = a})

-- | Information about a definition.
lcdrsDefinitions :: Lens' ListCoreDefinitionsResponse [DefinitionInformation]
lcdrsDefinitions = lens _lcdrsDefinitions (\ s a -> s{_lcdrsDefinitions = a}) . _Default . _Coerce

-- | -- | The response status code.
lcdrsResponseStatus :: Lens' ListCoreDefinitionsResponse Int
lcdrsResponseStatus = lens _lcdrsResponseStatus (\ s a -> s{_lcdrsResponseStatus = a})

instance NFData ListCoreDefinitionsResponse where
