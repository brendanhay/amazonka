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
-- Module      : Network.AWS.Greengrass.ListSubscriptionDefinitions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of subscription definitions.
module Network.AWS.Greengrass.ListSubscriptionDefinitions
    (
    -- * Creating a Request
      listSubscriptionDefinitions
    , ListSubscriptionDefinitions
    -- * Request Lenses
    , lsdNextToken
    , lsdMaxResults

    -- * Destructuring the Response
    , listSubscriptionDefinitionsResponse
    , ListSubscriptionDefinitionsResponse
    -- * Response Lenses
    , lsdrsNextToken
    , lsdrsDefinitions
    , lsdrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listSubscriptionDefinitions' smart constructor.
data ListSubscriptionDefinitions = ListSubscriptionDefinitions'
  { _lsdNextToken  :: !(Maybe Text)
  , _lsdMaxResults :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSubscriptionDefinitions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsdNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'lsdMaxResults' - The maximum number of results to be returned per request.
listSubscriptionDefinitions
    :: ListSubscriptionDefinitions
listSubscriptionDefinitions =
  ListSubscriptionDefinitions'
    {_lsdNextToken = Nothing, _lsdMaxResults = Nothing}


-- | The token for the next set of results, or ''null'' if there are no additional results.
lsdNextToken :: Lens' ListSubscriptionDefinitions (Maybe Text)
lsdNextToken = lens _lsdNextToken (\ s a -> s{_lsdNextToken = a})

-- | The maximum number of results to be returned per request.
lsdMaxResults :: Lens' ListSubscriptionDefinitions (Maybe Text)
lsdMaxResults = lens _lsdMaxResults (\ s a -> s{_lsdMaxResults = a})

instance AWSRequest ListSubscriptionDefinitions where
        type Rs ListSubscriptionDefinitions =
             ListSubscriptionDefinitionsResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 ListSubscriptionDefinitionsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "Definitions" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListSubscriptionDefinitions where

instance NFData ListSubscriptionDefinitions where

instance ToHeaders ListSubscriptionDefinitions where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListSubscriptionDefinitions where
        toPath = const "/greengrass/definition/subscriptions"

instance ToQuery ListSubscriptionDefinitions where
        toQuery ListSubscriptionDefinitions'{..}
          = mconcat
              ["NextToken" =: _lsdNextToken,
               "MaxResults" =: _lsdMaxResults]

-- | /See:/ 'listSubscriptionDefinitionsResponse' smart constructor.
data ListSubscriptionDefinitionsResponse = ListSubscriptionDefinitionsResponse'
  { _lsdrsNextToken      :: !(Maybe Text)
  , _lsdrsDefinitions    :: !(Maybe [DefinitionInformation])
  , _lsdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSubscriptionDefinitionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsdrsNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'lsdrsDefinitions' - Information about a definition.
--
-- * 'lsdrsResponseStatus' - -- | The response status code.
listSubscriptionDefinitionsResponse
    :: Int -- ^ 'lsdrsResponseStatus'
    -> ListSubscriptionDefinitionsResponse
listSubscriptionDefinitionsResponse pResponseStatus_ =
  ListSubscriptionDefinitionsResponse'
    { _lsdrsNextToken = Nothing
    , _lsdrsDefinitions = Nothing
    , _lsdrsResponseStatus = pResponseStatus_
    }


-- | The token for the next set of results, or ''null'' if there are no additional results.
lsdrsNextToken :: Lens' ListSubscriptionDefinitionsResponse (Maybe Text)
lsdrsNextToken = lens _lsdrsNextToken (\ s a -> s{_lsdrsNextToken = a})

-- | Information about a definition.
lsdrsDefinitions :: Lens' ListSubscriptionDefinitionsResponse [DefinitionInformation]
lsdrsDefinitions = lens _lsdrsDefinitions (\ s a -> s{_lsdrsDefinitions = a}) . _Default . _Coerce

-- | -- | The response status code.
lsdrsResponseStatus :: Lens' ListSubscriptionDefinitionsResponse Int
lsdrsResponseStatus = lens _lsdrsResponseStatus (\ s a -> s{_lsdrsResponseStatus = a})

instance NFData ListSubscriptionDefinitionsResponse
         where
