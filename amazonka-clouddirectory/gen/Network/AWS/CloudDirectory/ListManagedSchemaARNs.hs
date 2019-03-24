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
-- Module      : Network.AWS.CloudDirectory.ListManagedSchemaARNs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the major version families of each managed schema. If a major version ARN is provided as SchemaArn, the minor version revisions in that family are listed instead.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListManagedSchemaARNs
    (
    -- * Creating a Request
      listManagedSchemaARNs
    , ListManagedSchemaARNs
    -- * Request Lenses
    , lmsaNextToken
    , lmsaSchemaARN
    , lmsaMaxResults

    -- * Destructuring the Response
    , listManagedSchemaARNsResponse
    , ListManagedSchemaARNsResponse
    -- * Response Lenses
    , lmsarsSchemaARNs
    , lmsarsNextToken
    , lmsarsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listManagedSchemaARNs' smart constructor.
data ListManagedSchemaARNs = ListManagedSchemaARNs'
  { _lmsaNextToken  :: !(Maybe Text)
  , _lmsaSchemaARN  :: !(Maybe Text)
  , _lmsaMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListManagedSchemaARNs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmsaNextToken' - The pagination token.
--
-- * 'lmsaSchemaARN' - The response for ListManagedSchemaArns. When this parameter is used, all minor version ARNs for a major version are listed.
--
-- * 'lmsaMaxResults' - The maximum number of results to retrieve.
listManagedSchemaARNs
    :: ListManagedSchemaARNs
listManagedSchemaARNs =
  ListManagedSchemaARNs'
    { _lmsaNextToken = Nothing
    , _lmsaSchemaARN = Nothing
    , _lmsaMaxResults = Nothing
    }


-- | The pagination token.
lmsaNextToken :: Lens' ListManagedSchemaARNs (Maybe Text)
lmsaNextToken = lens _lmsaNextToken (\ s a -> s{_lmsaNextToken = a})

-- | The response for ListManagedSchemaArns. When this parameter is used, all minor version ARNs for a major version are listed.
lmsaSchemaARN :: Lens' ListManagedSchemaARNs (Maybe Text)
lmsaSchemaARN = lens _lmsaSchemaARN (\ s a -> s{_lmsaSchemaARN = a})

-- | The maximum number of results to retrieve.
lmsaMaxResults :: Lens' ListManagedSchemaARNs (Maybe Natural)
lmsaMaxResults = lens _lmsaMaxResults (\ s a -> s{_lmsaMaxResults = a}) . mapping _Nat

instance AWSPager ListManagedSchemaARNs where
        page rq rs
          | stop (rs ^. lmsarsNextToken) = Nothing
          | stop (rs ^. lmsarsSchemaARNs) = Nothing
          | otherwise =
            Just $ rq & lmsaNextToken .~ rs ^. lmsarsNextToken

instance AWSRequest ListManagedSchemaARNs where
        type Rs ListManagedSchemaARNs =
             ListManagedSchemaARNsResponse
        request = postJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 ListManagedSchemaARNsResponse' <$>
                   (x .?> "SchemaArns" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListManagedSchemaARNs where

instance NFData ListManagedSchemaARNs where

instance ToHeaders ListManagedSchemaARNs where
        toHeaders = const mempty

instance ToJSON ListManagedSchemaARNs where
        toJSON ListManagedSchemaARNs'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lmsaNextToken,
                  ("SchemaArn" .=) <$> _lmsaSchemaARN,
                  ("MaxResults" .=) <$> _lmsaMaxResults])

instance ToPath ListManagedSchemaARNs where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/schema/managed"

instance ToQuery ListManagedSchemaARNs where
        toQuery = const mempty

-- | /See:/ 'listManagedSchemaARNsResponse' smart constructor.
data ListManagedSchemaARNsResponse = ListManagedSchemaARNsResponse'
  { _lmsarsSchemaARNs     :: !(Maybe [Text])
  , _lmsarsNextToken      :: !(Maybe Text)
  , _lmsarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListManagedSchemaARNsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmsarsSchemaARNs' - The ARNs for all AWS managed schemas.
--
-- * 'lmsarsNextToken' - The pagination token.
--
-- * 'lmsarsResponseStatus' - -- | The response status code.
listManagedSchemaARNsResponse
    :: Int -- ^ 'lmsarsResponseStatus'
    -> ListManagedSchemaARNsResponse
listManagedSchemaARNsResponse pResponseStatus_ =
  ListManagedSchemaARNsResponse'
    { _lmsarsSchemaARNs = Nothing
    , _lmsarsNextToken = Nothing
    , _lmsarsResponseStatus = pResponseStatus_
    }


-- | The ARNs for all AWS managed schemas.
lmsarsSchemaARNs :: Lens' ListManagedSchemaARNsResponse [Text]
lmsarsSchemaARNs = lens _lmsarsSchemaARNs (\ s a -> s{_lmsarsSchemaARNs = a}) . _Default . _Coerce

-- | The pagination token.
lmsarsNextToken :: Lens' ListManagedSchemaARNsResponse (Maybe Text)
lmsarsNextToken = lens _lmsarsNextToken (\ s a -> s{_lmsarsNextToken = a})

-- | -- | The response status code.
lmsarsResponseStatus :: Lens' ListManagedSchemaARNsResponse Int
lmsarsResponseStatus = lens _lmsarsResponseStatus (\ s a -> s{_lmsarsResponseStatus = a})

instance NFData ListManagedSchemaARNsResponse where
