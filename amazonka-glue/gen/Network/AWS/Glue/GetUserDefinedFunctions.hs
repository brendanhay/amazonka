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
-- Module      : Network.AWS.Glue.GetUserDefinedFunctions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a multiple function definitions from the Data Catalog.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetUserDefinedFunctions
    (
    -- * Creating a Request
      getUserDefinedFunctions
    , GetUserDefinedFunctions
    -- * Request Lenses
    , gudfCatalogId
    , gudfNextToken
    , gudfMaxResults
    , gudfDatabaseName
    , gudfPattern

    -- * Destructuring the Response
    , getUserDefinedFunctionsResponse
    , GetUserDefinedFunctionsResponse
    -- * Response Lenses
    , gudfrsNextToken
    , gudfrsUserDefinedFunctions
    , gudfrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getUserDefinedFunctions' smart constructor.
data GetUserDefinedFunctions = GetUserDefinedFunctions'
  { _gudfCatalogId    :: !(Maybe Text)
  , _gudfNextToken    :: !(Maybe Text)
  , _gudfMaxResults   :: !(Maybe Nat)
  , _gudfDatabaseName :: !Text
  , _gudfPattern      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetUserDefinedFunctions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gudfCatalogId' - The ID of the Data Catalog where the functions to be retrieved are located. If none is supplied, the AWS account ID is used by default.
--
-- * 'gudfNextToken' - A continuation token, if this is a continuation call.
--
-- * 'gudfMaxResults' - The maximum number of functions to return in one response.
--
-- * 'gudfDatabaseName' - The name of the catalog database where the functions are located.
--
-- * 'gudfPattern' - An optional function-name pattern string that filters the function definitions returned.
getUserDefinedFunctions
    :: Text -- ^ 'gudfDatabaseName'
    -> Text -- ^ 'gudfPattern'
    -> GetUserDefinedFunctions
getUserDefinedFunctions pDatabaseName_ pPattern_ =
  GetUserDefinedFunctions'
    { _gudfCatalogId = Nothing
    , _gudfNextToken = Nothing
    , _gudfMaxResults = Nothing
    , _gudfDatabaseName = pDatabaseName_
    , _gudfPattern = pPattern_
    }


-- | The ID of the Data Catalog where the functions to be retrieved are located. If none is supplied, the AWS account ID is used by default.
gudfCatalogId :: Lens' GetUserDefinedFunctions (Maybe Text)
gudfCatalogId = lens _gudfCatalogId (\ s a -> s{_gudfCatalogId = a})

-- | A continuation token, if this is a continuation call.
gudfNextToken :: Lens' GetUserDefinedFunctions (Maybe Text)
gudfNextToken = lens _gudfNextToken (\ s a -> s{_gudfNextToken = a})

-- | The maximum number of functions to return in one response.
gudfMaxResults :: Lens' GetUserDefinedFunctions (Maybe Natural)
gudfMaxResults = lens _gudfMaxResults (\ s a -> s{_gudfMaxResults = a}) . mapping _Nat

-- | The name of the catalog database where the functions are located.
gudfDatabaseName :: Lens' GetUserDefinedFunctions Text
gudfDatabaseName = lens _gudfDatabaseName (\ s a -> s{_gudfDatabaseName = a})

-- | An optional function-name pattern string that filters the function definitions returned.
gudfPattern :: Lens' GetUserDefinedFunctions Text
gudfPattern = lens _gudfPattern (\ s a -> s{_gudfPattern = a})

instance AWSPager GetUserDefinedFunctions where
        page rq rs
          | stop (rs ^. gudfrsNextToken) = Nothing
          | stop (rs ^. gudfrsUserDefinedFunctions) = Nothing
          | otherwise =
            Just $ rq & gudfNextToken .~ rs ^. gudfrsNextToken

instance AWSRequest GetUserDefinedFunctions where
        type Rs GetUserDefinedFunctions =
             GetUserDefinedFunctionsResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 GetUserDefinedFunctionsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "UserDefinedFunctions" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetUserDefinedFunctions where

instance NFData GetUserDefinedFunctions where

instance ToHeaders GetUserDefinedFunctions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.GetUserDefinedFunctions" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetUserDefinedFunctions where
        toJSON GetUserDefinedFunctions'{..}
          = object
              (catMaybes
                 [("CatalogId" .=) <$> _gudfCatalogId,
                  ("NextToken" .=) <$> _gudfNextToken,
                  ("MaxResults" .=) <$> _gudfMaxResults,
                  Just ("DatabaseName" .= _gudfDatabaseName),
                  Just ("Pattern" .= _gudfPattern)])

instance ToPath GetUserDefinedFunctions where
        toPath = const "/"

instance ToQuery GetUserDefinedFunctions where
        toQuery = const mempty

-- | /See:/ 'getUserDefinedFunctionsResponse' smart constructor.
data GetUserDefinedFunctionsResponse = GetUserDefinedFunctionsResponse'
  { _gudfrsNextToken            :: !(Maybe Text)
  , _gudfrsUserDefinedFunctions :: !(Maybe [UserDefinedFunction])
  , _gudfrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetUserDefinedFunctionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gudfrsNextToken' - A continuation token, if the list of functions returned does not include the last requested function.
--
-- * 'gudfrsUserDefinedFunctions' - A list of requested function definitions.
--
-- * 'gudfrsResponseStatus' - -- | The response status code.
getUserDefinedFunctionsResponse
    :: Int -- ^ 'gudfrsResponseStatus'
    -> GetUserDefinedFunctionsResponse
getUserDefinedFunctionsResponse pResponseStatus_ =
  GetUserDefinedFunctionsResponse'
    { _gudfrsNextToken = Nothing
    , _gudfrsUserDefinedFunctions = Nothing
    , _gudfrsResponseStatus = pResponseStatus_
    }


-- | A continuation token, if the list of functions returned does not include the last requested function.
gudfrsNextToken :: Lens' GetUserDefinedFunctionsResponse (Maybe Text)
gudfrsNextToken = lens _gudfrsNextToken (\ s a -> s{_gudfrsNextToken = a})

-- | A list of requested function definitions.
gudfrsUserDefinedFunctions :: Lens' GetUserDefinedFunctionsResponse [UserDefinedFunction]
gudfrsUserDefinedFunctions = lens _gudfrsUserDefinedFunctions (\ s a -> s{_gudfrsUserDefinedFunctions = a}) . _Default . _Coerce

-- | -- | The response status code.
gudfrsResponseStatus :: Lens' GetUserDefinedFunctionsResponse Int
gudfrsResponseStatus = lens _gudfrsResponseStatus (\ s a -> s{_gudfrsResponseStatus = a})

instance NFData GetUserDefinedFunctionsResponse where
