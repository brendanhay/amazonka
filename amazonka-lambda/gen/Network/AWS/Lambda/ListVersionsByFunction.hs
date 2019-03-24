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
-- Module      : Network.AWS.Lambda.ListVersionsByFunction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of <https://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html versions> , with the version-specific configuration of each.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListVersionsByFunction
    (
    -- * Creating a Request
      listVersionsByFunction
    , ListVersionsByFunction
    -- * Request Lenses
    , lvbfMarker
    , lvbfMaxItems
    , lvbfFunctionName

    -- * Destructuring the Response
    , listVersionsByFunctionResponse
    , ListVersionsByFunctionResponse
    -- * Response Lenses
    , lvbfrsVersions
    , lvbfrsNextMarker
    , lvbfrsResponseStatus
    ) where

import Network.AWS.Lambda.Types
import Network.AWS.Lambda.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listVersionsByFunction' smart constructor.
data ListVersionsByFunction = ListVersionsByFunction'
  { _lvbfMarker       :: !(Maybe Text)
  , _lvbfMaxItems     :: !(Maybe Nat)
  , _lvbfFunctionName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListVersionsByFunction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lvbfMarker' - Specify the pagination token that's returned by a previous request to retrieve the next page of results.
--
-- * 'lvbfMaxItems' - Limit the number of versions that are returned.
--
-- * 'lvbfFunctionName' - The name of the Lambda function. __Name formats__      * __Function name__ - @MyFunction@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .     * __Partial ARN__ - @123456789012:function:MyFunction@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
listVersionsByFunction
    :: Text -- ^ 'lvbfFunctionName'
    -> ListVersionsByFunction
listVersionsByFunction pFunctionName_ =
  ListVersionsByFunction'
    { _lvbfMarker = Nothing
    , _lvbfMaxItems = Nothing
    , _lvbfFunctionName = pFunctionName_
    }


-- | Specify the pagination token that's returned by a previous request to retrieve the next page of results.
lvbfMarker :: Lens' ListVersionsByFunction (Maybe Text)
lvbfMarker = lens _lvbfMarker (\ s a -> s{_lvbfMarker = a})

-- | Limit the number of versions that are returned.
lvbfMaxItems :: Lens' ListVersionsByFunction (Maybe Natural)
lvbfMaxItems = lens _lvbfMaxItems (\ s a -> s{_lvbfMaxItems = a}) . mapping _Nat

-- | The name of the Lambda function. __Name formats__      * __Function name__ - @MyFunction@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .     * __Partial ARN__ - @123456789012:function:MyFunction@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
lvbfFunctionName :: Lens' ListVersionsByFunction Text
lvbfFunctionName = lens _lvbfFunctionName (\ s a -> s{_lvbfFunctionName = a})

instance AWSPager ListVersionsByFunction where
        page rq rs
          | stop (rs ^. lvbfrsNextMarker) = Nothing
          | stop (rs ^. lvbfrsVersions) = Nothing
          | otherwise =
            Just $ rq & lvbfMarker .~ rs ^. lvbfrsNextMarker

instance AWSRequest ListVersionsByFunction where
        type Rs ListVersionsByFunction =
             ListVersionsByFunctionResponse
        request = get lambda
        response
          = receiveJSON
              (\ s h x ->
                 ListVersionsByFunctionResponse' <$>
                   (x .?> "Versions" .!@ mempty) <*>
                     (x .?> "NextMarker")
                     <*> (pure (fromEnum s)))

instance Hashable ListVersionsByFunction where

instance NFData ListVersionsByFunction where

instance ToHeaders ListVersionsByFunction where
        toHeaders = const mempty

instance ToPath ListVersionsByFunction where
        toPath ListVersionsByFunction'{..}
          = mconcat
              ["/2015-03-31/functions/", toBS _lvbfFunctionName,
               "/versions"]

instance ToQuery ListVersionsByFunction where
        toQuery ListVersionsByFunction'{..}
          = mconcat
              ["Marker" =: _lvbfMarker,
               "MaxItems" =: _lvbfMaxItems]

-- | /See:/ 'listVersionsByFunctionResponse' smart constructor.
data ListVersionsByFunctionResponse = ListVersionsByFunctionResponse'
  { _lvbfrsVersions       :: !(Maybe [FunctionConfiguration])
  , _lvbfrsNextMarker     :: !(Maybe Text)
  , _lvbfrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListVersionsByFunctionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lvbfrsVersions' - A list of Lambda function versions.
--
-- * 'lvbfrsNextMarker' - The pagination token that's included if more results are available.
--
-- * 'lvbfrsResponseStatus' - -- | The response status code.
listVersionsByFunctionResponse
    :: Int -- ^ 'lvbfrsResponseStatus'
    -> ListVersionsByFunctionResponse
listVersionsByFunctionResponse pResponseStatus_ =
  ListVersionsByFunctionResponse'
    { _lvbfrsVersions = Nothing
    , _lvbfrsNextMarker = Nothing
    , _lvbfrsResponseStatus = pResponseStatus_
    }


-- | A list of Lambda function versions.
lvbfrsVersions :: Lens' ListVersionsByFunctionResponse [FunctionConfiguration]
lvbfrsVersions = lens _lvbfrsVersions (\ s a -> s{_lvbfrsVersions = a}) . _Default . _Coerce

-- | The pagination token that's included if more results are available.
lvbfrsNextMarker :: Lens' ListVersionsByFunctionResponse (Maybe Text)
lvbfrsNextMarker = lens _lvbfrsNextMarker (\ s a -> s{_lvbfrsNextMarker = a})

-- | -- | The response status code.
lvbfrsResponseStatus :: Lens' ListVersionsByFunctionResponse Int
lvbfrsResponseStatus = lens _lvbfrsResponseStatus (\ s a -> s{_lvbfrsResponseStatus = a})

instance NFData ListVersionsByFunctionResponse where
