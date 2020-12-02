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
-- Module      : Network.AWS.Greengrass.ListResourceDefinitions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of resource definitions.
module Network.AWS.Greengrass.ListResourceDefinitions
    (
    -- * Creating a Request
      listResourceDefinitions
    , ListResourceDefinitions
    -- * Request Lenses
    , lrdNextToken
    , lrdMaxResults

    -- * Destructuring the Response
    , listResourceDefinitionsResponse
    , ListResourceDefinitionsResponse
    -- * Response Lenses
    , lrdrsNextToken
    , lrdrsDefinitions
    , lrdrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listResourceDefinitions' smart constructor.
data ListResourceDefinitions = ListResourceDefinitions'
  { _lrdNextToken  :: !(Maybe Text)
  , _lrdMaxResults :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListResourceDefinitions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrdNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'lrdMaxResults' - The maximum number of results to be returned per request.
listResourceDefinitions
    :: ListResourceDefinitions
listResourceDefinitions =
  ListResourceDefinitions' {_lrdNextToken = Nothing, _lrdMaxResults = Nothing}


-- | The token for the next set of results, or ''null'' if there are no additional results.
lrdNextToken :: Lens' ListResourceDefinitions (Maybe Text)
lrdNextToken = lens _lrdNextToken (\ s a -> s{_lrdNextToken = a})

-- | The maximum number of results to be returned per request.
lrdMaxResults :: Lens' ListResourceDefinitions (Maybe Text)
lrdMaxResults = lens _lrdMaxResults (\ s a -> s{_lrdMaxResults = a})

instance AWSRequest ListResourceDefinitions where
        type Rs ListResourceDefinitions =
             ListResourceDefinitionsResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 ListResourceDefinitionsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "Definitions" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListResourceDefinitions where

instance NFData ListResourceDefinitions where

instance ToHeaders ListResourceDefinitions where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListResourceDefinitions where
        toPath = const "/greengrass/definition/resources"

instance ToQuery ListResourceDefinitions where
        toQuery ListResourceDefinitions'{..}
          = mconcat
              ["NextToken" =: _lrdNextToken,
               "MaxResults" =: _lrdMaxResults]

-- | /See:/ 'listResourceDefinitionsResponse' smart constructor.
data ListResourceDefinitionsResponse = ListResourceDefinitionsResponse'
  { _lrdrsNextToken      :: !(Maybe Text)
  , _lrdrsDefinitions    :: !(Maybe [DefinitionInformation])
  , _lrdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListResourceDefinitionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrdrsNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'lrdrsDefinitions' - Information about a definition.
--
-- * 'lrdrsResponseStatus' - -- | The response status code.
listResourceDefinitionsResponse
    :: Int -- ^ 'lrdrsResponseStatus'
    -> ListResourceDefinitionsResponse
listResourceDefinitionsResponse pResponseStatus_ =
  ListResourceDefinitionsResponse'
    { _lrdrsNextToken = Nothing
    , _lrdrsDefinitions = Nothing
    , _lrdrsResponseStatus = pResponseStatus_
    }


-- | The token for the next set of results, or ''null'' if there are no additional results.
lrdrsNextToken :: Lens' ListResourceDefinitionsResponse (Maybe Text)
lrdrsNextToken = lens _lrdrsNextToken (\ s a -> s{_lrdrsNextToken = a})

-- | Information about a definition.
lrdrsDefinitions :: Lens' ListResourceDefinitionsResponse [DefinitionInformation]
lrdrsDefinitions = lens _lrdrsDefinitions (\ s a -> s{_lrdrsDefinitions = a}) . _Default . _Coerce

-- | -- | The response status code.
lrdrsResponseStatus :: Lens' ListResourceDefinitionsResponse Int
lrdrsResponseStatus = lens _lrdrsResponseStatus (\ s a -> s{_lrdrsResponseStatus = a})

instance NFData ListResourceDefinitionsResponse where
