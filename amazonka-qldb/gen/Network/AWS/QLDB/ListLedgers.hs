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
-- Module      : Network.AWS.QLDB.ListLedgers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
--
-- This operation returns paginated results.
module Network.AWS.QLDB.ListLedgers
    (
    -- * Creating a Request
      listLedgers
    , ListLedgers
    -- * Request Lenses
    , llNextToken
    , llMaxResults

    -- * Destructuring the Response
    , listLedgersResponse
    , ListLedgersResponse
    -- * Response Lenses
    , llrsLedgers
    , llrsNextToken
    , llrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.QLDB.Types
import Network.AWS.QLDB.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listLedgers' smart constructor.
data ListLedgers = ListLedgers'
  { _llNextToken  :: !(Maybe Text)
  , _llMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListLedgers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'llNextToken' - Undocumented member.
--
-- * 'llMaxResults' - Undocumented member.
listLedgers
    :: ListLedgers
listLedgers = ListLedgers' {_llNextToken = Nothing, _llMaxResults = Nothing}


-- | Undocumented member.
llNextToken :: Lens' ListLedgers (Maybe Text)
llNextToken = lens _llNextToken (\ s a -> s{_llNextToken = a})

-- | Undocumented member.
llMaxResults :: Lens' ListLedgers (Maybe Natural)
llMaxResults = lens _llMaxResults (\ s a -> s{_llMaxResults = a}) . mapping _Nat

instance AWSPager ListLedgers where
        page rq rs
          | stop (rs ^. llrsNextToken) = Nothing
          | otherwise =
            Just $ rq & llNextToken .~ rs ^. llrsNextToken

instance AWSRequest ListLedgers where
        type Rs ListLedgers = ListLedgersResponse
        request = get qldb
        response
          = receiveJSON
              (\ s h x ->
                 ListLedgersResponse' <$>
                   (x .?> "Ledgers" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListLedgers where

instance NFData ListLedgers where

instance ToHeaders ListLedgers where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToPath ListLedgers where
        toPath = const "/ledgers"

instance ToQuery ListLedgers where
        toQuery ListLedgers'{..}
          = mconcat
              ["next_token" =: _llNextToken,
               "max_results" =: _llMaxResults]

-- | /See:/ 'listLedgersResponse' smart constructor.
data ListLedgersResponse = ListLedgersResponse'
  { _llrsLedgers        :: !(Maybe [LedgerSummary])
  , _llrsNextToken      :: !(Maybe Text)
  , _llrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListLedgersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'llrsLedgers' - Undocumented member.
--
-- * 'llrsNextToken' - Undocumented member.
--
-- * 'llrsResponseStatus' - -- | The response status code.
listLedgersResponse
    :: Int -- ^ 'llrsResponseStatus'
    -> ListLedgersResponse
listLedgersResponse pResponseStatus_ =
  ListLedgersResponse'
    { _llrsLedgers = Nothing
    , _llrsNextToken = Nothing
    , _llrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
llrsLedgers :: Lens' ListLedgersResponse [LedgerSummary]
llrsLedgers = lens _llrsLedgers (\ s a -> s{_llrsLedgers = a}) . _Default . _Coerce

-- | Undocumented member.
llrsNextToken :: Lens' ListLedgersResponse (Maybe Text)
llrsNextToken = lens _llrsNextToken (\ s a -> s{_llrsNextToken = a})

-- | -- | The response status code.
llrsResponseStatus :: Lens' ListLedgersResponse Int
llrsResponseStatus = lens _llrsResponseStatus (\ s a -> s{_llrsResponseStatus = a})

instance NFData ListLedgersResponse where
