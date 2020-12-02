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
-- Module      : Network.AWS.Glue.GetClassifiers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all classifier objects in the Data Catalog.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetClassifiers
    (
    -- * Creating a Request
      getClassifiers
    , GetClassifiers
    -- * Request Lenses
    , gcNextToken
    , gcMaxResults

    -- * Destructuring the Response
    , getClassifiersResponse
    , GetClassifiersResponse
    -- * Response Lenses
    , gcsrsNextToken
    , gcsrsClassifiers
    , gcsrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getClassifiers' smart constructor.
data GetClassifiers = GetClassifiers'
  { _gcNextToken  :: !(Maybe Text)
  , _gcMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetClassifiers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcNextToken' - An optional continuation token.
--
-- * 'gcMaxResults' - Size of the list to return (optional).
getClassifiers
    :: GetClassifiers
getClassifiers =
  GetClassifiers' {_gcNextToken = Nothing, _gcMaxResults = Nothing}


-- | An optional continuation token.
gcNextToken :: Lens' GetClassifiers (Maybe Text)
gcNextToken = lens _gcNextToken (\ s a -> s{_gcNextToken = a})

-- | Size of the list to return (optional).
gcMaxResults :: Lens' GetClassifiers (Maybe Natural)
gcMaxResults = lens _gcMaxResults (\ s a -> s{_gcMaxResults = a}) . mapping _Nat

instance AWSPager GetClassifiers where
        page rq rs
          | stop (rs ^. gcsrsNextToken) = Nothing
          | stop (rs ^. gcsrsClassifiers) = Nothing
          | otherwise =
            Just $ rq & gcNextToken .~ rs ^. gcsrsNextToken

instance AWSRequest GetClassifiers where
        type Rs GetClassifiers = GetClassifiersResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 GetClassifiersResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "Classifiers" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetClassifiers where

instance NFData GetClassifiers where

instance ToHeaders GetClassifiers where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.GetClassifiers" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetClassifiers where
        toJSON GetClassifiers'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _gcNextToken,
                  ("MaxResults" .=) <$> _gcMaxResults])

instance ToPath GetClassifiers where
        toPath = const "/"

instance ToQuery GetClassifiers where
        toQuery = const mempty

-- | /See:/ 'getClassifiersResponse' smart constructor.
data GetClassifiersResponse = GetClassifiersResponse'
  { _gcsrsNextToken      :: !(Maybe Text)
  , _gcsrsClassifiers    :: !(Maybe [Classifier])
  , _gcsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetClassifiersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcsrsNextToken' - A continuation token.
--
-- * 'gcsrsClassifiers' - The requested list of classifier objects.
--
-- * 'gcsrsResponseStatus' - -- | The response status code.
getClassifiersResponse
    :: Int -- ^ 'gcsrsResponseStatus'
    -> GetClassifiersResponse
getClassifiersResponse pResponseStatus_ =
  GetClassifiersResponse'
    { _gcsrsNextToken = Nothing
    , _gcsrsClassifiers = Nothing
    , _gcsrsResponseStatus = pResponseStatus_
    }


-- | A continuation token.
gcsrsNextToken :: Lens' GetClassifiersResponse (Maybe Text)
gcsrsNextToken = lens _gcsrsNextToken (\ s a -> s{_gcsrsNextToken = a})

-- | The requested list of classifier objects.
gcsrsClassifiers :: Lens' GetClassifiersResponse [Classifier]
gcsrsClassifiers = lens _gcsrsClassifiers (\ s a -> s{_gcsrsClassifiers = a}) . _Default . _Coerce

-- | -- | The response status code.
gcsrsResponseStatus :: Lens' GetClassifiersResponse Int
gcsrsResponseStatus = lens _gcsrsResponseStatus (\ s a -> s{_gcsrsResponseStatus = a})

instance NFData GetClassifiersResponse where
