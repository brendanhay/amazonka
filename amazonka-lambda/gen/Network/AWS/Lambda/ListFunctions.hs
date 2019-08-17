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
-- Module      : Network.AWS.Lambda.ListFunctions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of Lambda functions, with the version-specific configuration of each.
--
--
-- Set @FunctionVersion@ to @ALL@ to include all published versions of each function in addition to the unpublished version. To get more information about a function or version, use 'GetFunction' .
--
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListFunctions
    (
    -- * Creating a Request
      listFunctions
    , ListFunctions
    -- * Request Lenses
    , lfMasterRegion
    , lfMarker
    , lfMaxItems
    , lfFunctionVersion

    -- * Destructuring the Response
    , listFunctionsResponse
    , ListFunctionsResponse
    -- * Response Lenses
    , lfrsNextMarker
    , lfrsFunctions
    , lfrsResponseStatus
    ) where

import Network.AWS.Lambda.Types
import Network.AWS.Lambda.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listFunctions' smart constructor.
data ListFunctions = ListFunctions'
  { _lfMasterRegion    :: !(Maybe Text)
  , _lfMarker          :: !(Maybe Text)
  , _lfMaxItems        :: !(Maybe Nat)
  , _lfFunctionVersion :: !(Maybe FunctionVersion)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListFunctions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfMasterRegion' - For Lambda@Edge functions, the AWS Region of the master function. For example, @us-east-2@ or @ALL@ . If specified, you must set @FunctionVersion@ to @ALL@ .
--
-- * 'lfMarker' - Specify the pagination token that's returned by a previous request to retrieve the next page of results.
--
-- * 'lfMaxItems' - Specify a value between 1 and 50 to limit the number of functions in the response.
--
-- * 'lfFunctionVersion' - Set to @ALL@ to include entries for all published versions of each function.
listFunctions
    :: ListFunctions
listFunctions =
  ListFunctions'
    { _lfMasterRegion = Nothing
    , _lfMarker = Nothing
    , _lfMaxItems = Nothing
    , _lfFunctionVersion = Nothing
    }


-- | For Lambda@Edge functions, the AWS Region of the master function. For example, @us-east-2@ or @ALL@ . If specified, you must set @FunctionVersion@ to @ALL@ .
lfMasterRegion :: Lens' ListFunctions (Maybe Text)
lfMasterRegion = lens _lfMasterRegion (\ s a -> s{_lfMasterRegion = a})

-- | Specify the pagination token that's returned by a previous request to retrieve the next page of results.
lfMarker :: Lens' ListFunctions (Maybe Text)
lfMarker = lens _lfMarker (\ s a -> s{_lfMarker = a})

-- | Specify a value between 1 and 50 to limit the number of functions in the response.
lfMaxItems :: Lens' ListFunctions (Maybe Natural)
lfMaxItems = lens _lfMaxItems (\ s a -> s{_lfMaxItems = a}) . mapping _Nat

-- | Set to @ALL@ to include entries for all published versions of each function.
lfFunctionVersion :: Lens' ListFunctions (Maybe FunctionVersion)
lfFunctionVersion = lens _lfFunctionVersion (\ s a -> s{_lfFunctionVersion = a})

instance AWSPager ListFunctions where
        page rq rs
          | stop (rs ^. lfrsNextMarker) = Nothing
          | stop (rs ^. lfrsFunctions) = Nothing
          | otherwise =
            Just $ rq & lfMarker .~ rs ^. lfrsNextMarker

instance AWSRequest ListFunctions where
        type Rs ListFunctions = ListFunctionsResponse
        request = get lambda
        response
          = receiveJSON
              (\ s h x ->
                 ListFunctionsResponse' <$>
                   (x .?> "NextMarker") <*>
                     (x .?> "Functions" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListFunctions where

instance NFData ListFunctions where

instance ToHeaders ListFunctions where
        toHeaders = const mempty

instance ToPath ListFunctions where
        toPath = const "/2015-03-31/functions/"

instance ToQuery ListFunctions where
        toQuery ListFunctions'{..}
          = mconcat
              ["MasterRegion" =: _lfMasterRegion,
               "Marker" =: _lfMarker, "MaxItems" =: _lfMaxItems,
               "FunctionVersion" =: _lfFunctionVersion]

-- | A list of Lambda functions.
--
--
--
-- /See:/ 'listFunctionsResponse' smart constructor.
data ListFunctionsResponse = ListFunctionsResponse'
  { _lfrsNextMarker     :: !(Maybe Text)
  , _lfrsFunctions      :: !(Maybe [FunctionConfiguration])
  , _lfrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListFunctionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfrsNextMarker' - The pagination token that's included if more results are available.
--
-- * 'lfrsFunctions' - A list of Lambda functions.
--
-- * 'lfrsResponseStatus' - -- | The response status code.
listFunctionsResponse
    :: Int -- ^ 'lfrsResponseStatus'
    -> ListFunctionsResponse
listFunctionsResponse pResponseStatus_ =
  ListFunctionsResponse'
    { _lfrsNextMarker = Nothing
    , _lfrsFunctions = Nothing
    , _lfrsResponseStatus = pResponseStatus_
    }


-- | The pagination token that's included if more results are available.
lfrsNextMarker :: Lens' ListFunctionsResponse (Maybe Text)
lfrsNextMarker = lens _lfrsNextMarker (\ s a -> s{_lfrsNextMarker = a})

-- | A list of Lambda functions.
lfrsFunctions :: Lens' ListFunctionsResponse [FunctionConfiguration]
lfrsFunctions = lens _lfrsFunctions (\ s a -> s{_lfrsFunctions = a}) . _Default . _Coerce

-- | -- | The response status code.
lfrsResponseStatus :: Lens' ListFunctionsResponse Int
lfrsResponseStatus = lens _lfrsResponseStatus (\ s a -> s{_lfrsResponseStatus = a})

instance NFData ListFunctionsResponse where
