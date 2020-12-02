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
-- Module      : Network.AWS.Greengrass.ListDeviceDefinitions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of device definitions.
module Network.AWS.Greengrass.ListDeviceDefinitions
    (
    -- * Creating a Request
      listDeviceDefinitions
    , ListDeviceDefinitions
    -- * Request Lenses
    , lddNextToken
    , lddMaxResults

    -- * Destructuring the Response
    , listDeviceDefinitionsResponse
    , ListDeviceDefinitionsResponse
    -- * Response Lenses
    , lddrsNextToken
    , lddrsDefinitions
    , lddrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listDeviceDefinitions' smart constructor.
data ListDeviceDefinitions = ListDeviceDefinitions'
  { _lddNextToken  :: !(Maybe Text)
  , _lddMaxResults :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDeviceDefinitions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lddNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'lddMaxResults' - The maximum number of results to be returned per request.
listDeviceDefinitions
    :: ListDeviceDefinitions
listDeviceDefinitions =
  ListDeviceDefinitions' {_lddNextToken = Nothing, _lddMaxResults = Nothing}


-- | The token for the next set of results, or ''null'' if there are no additional results.
lddNextToken :: Lens' ListDeviceDefinitions (Maybe Text)
lddNextToken = lens _lddNextToken (\ s a -> s{_lddNextToken = a})

-- | The maximum number of results to be returned per request.
lddMaxResults :: Lens' ListDeviceDefinitions (Maybe Text)
lddMaxResults = lens _lddMaxResults (\ s a -> s{_lddMaxResults = a})

instance AWSRequest ListDeviceDefinitions where
        type Rs ListDeviceDefinitions =
             ListDeviceDefinitionsResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 ListDeviceDefinitionsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "Definitions" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListDeviceDefinitions where

instance NFData ListDeviceDefinitions where

instance ToHeaders ListDeviceDefinitions where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListDeviceDefinitions where
        toPath = const "/greengrass/definition/devices"

instance ToQuery ListDeviceDefinitions where
        toQuery ListDeviceDefinitions'{..}
          = mconcat
              ["NextToken" =: _lddNextToken,
               "MaxResults" =: _lddMaxResults]

-- | /See:/ 'listDeviceDefinitionsResponse' smart constructor.
data ListDeviceDefinitionsResponse = ListDeviceDefinitionsResponse'
  { _lddrsNextToken      :: !(Maybe Text)
  , _lddrsDefinitions    :: !(Maybe [DefinitionInformation])
  , _lddrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDeviceDefinitionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lddrsNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'lddrsDefinitions' - Information about a definition.
--
-- * 'lddrsResponseStatus' - -- | The response status code.
listDeviceDefinitionsResponse
    :: Int -- ^ 'lddrsResponseStatus'
    -> ListDeviceDefinitionsResponse
listDeviceDefinitionsResponse pResponseStatus_ =
  ListDeviceDefinitionsResponse'
    { _lddrsNextToken = Nothing
    , _lddrsDefinitions = Nothing
    , _lddrsResponseStatus = pResponseStatus_
    }


-- | The token for the next set of results, or ''null'' if there are no additional results.
lddrsNextToken :: Lens' ListDeviceDefinitionsResponse (Maybe Text)
lddrsNextToken = lens _lddrsNextToken (\ s a -> s{_lddrsNextToken = a})

-- | Information about a definition.
lddrsDefinitions :: Lens' ListDeviceDefinitionsResponse [DefinitionInformation]
lddrsDefinitions = lens _lddrsDefinitions (\ s a -> s{_lddrsDefinitions = a}) . _Default . _Coerce

-- | -- | The response status code.
lddrsResponseStatus :: Lens' ListDeviceDefinitionsResponse Int
lddrsResponseStatus = lens _lddrsResponseStatus (\ s a -> s{_lddrsResponseStatus = a})

instance NFData ListDeviceDefinitionsResponse where
