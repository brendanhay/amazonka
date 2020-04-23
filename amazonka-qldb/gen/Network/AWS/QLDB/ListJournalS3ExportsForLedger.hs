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
-- Module      : Network.AWS.QLDB.ListJournalS3ExportsForLedger
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
--
-- This operation returns paginated results.
module Network.AWS.QLDB.ListJournalS3ExportsForLedger
    (
    -- * Creating a Request
      listJournalS3ExportsForLedger
    , ListJournalS3ExportsForLedger
    -- * Request Lenses
    , ljseflNextToken
    , ljseflMaxResults
    , ljseflName

    -- * Destructuring the Response
    , listJournalS3ExportsForLedgerResponse
    , ListJournalS3ExportsForLedgerResponse
    -- * Response Lenses
    , ljseflrsJournalS3Exports
    , ljseflrsNextToken
    , ljseflrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.QLDB.Types
import Network.AWS.QLDB.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listJournalS3ExportsForLedger' smart constructor.
data ListJournalS3ExportsForLedger = ListJournalS3ExportsForLedger'
  { _ljseflNextToken  :: !(Maybe Text)
  , _ljseflMaxResults :: !(Maybe Nat)
  , _ljseflName       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListJournalS3ExportsForLedger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljseflNextToken' - Undocumented member.
--
-- * 'ljseflMaxResults' - Undocumented member.
--
-- * 'ljseflName' - Undocumented member.
listJournalS3ExportsForLedger
    :: Text -- ^ 'ljseflName'
    -> ListJournalS3ExportsForLedger
listJournalS3ExportsForLedger pName_ =
  ListJournalS3ExportsForLedger'
    { _ljseflNextToken = Nothing
    , _ljseflMaxResults = Nothing
    , _ljseflName = pName_
    }


-- | Undocumented member.
ljseflNextToken :: Lens' ListJournalS3ExportsForLedger (Maybe Text)
ljseflNextToken = lens _ljseflNextToken (\ s a -> s{_ljseflNextToken = a})

-- | Undocumented member.
ljseflMaxResults :: Lens' ListJournalS3ExportsForLedger (Maybe Natural)
ljseflMaxResults = lens _ljseflMaxResults (\ s a -> s{_ljseflMaxResults = a}) . mapping _Nat

-- | Undocumented member.
ljseflName :: Lens' ListJournalS3ExportsForLedger Text
ljseflName = lens _ljseflName (\ s a -> s{_ljseflName = a})

instance AWSPager ListJournalS3ExportsForLedger where
        page rq rs
          | stop (rs ^. ljseflrsNextToken) = Nothing
          | otherwise =
            Just $ rq &
              ljseflNextToken .~ rs ^. ljseflrsNextToken

instance AWSRequest ListJournalS3ExportsForLedger
         where
        type Rs ListJournalS3ExportsForLedger =
             ListJournalS3ExportsForLedgerResponse
        request = get qldb
        response
          = receiveJSON
              (\ s h x ->
                 ListJournalS3ExportsForLedgerResponse' <$>
                   (x .?> "JournalS3Exports" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListJournalS3ExportsForLedger where

instance NFData ListJournalS3ExportsForLedger where

instance ToHeaders ListJournalS3ExportsForLedger
         where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToPath ListJournalS3ExportsForLedger where
        toPath ListJournalS3ExportsForLedger'{..}
          = mconcat
              ["/ledgers/", toBS _ljseflName,
               "/journal-s3-exports"]

instance ToQuery ListJournalS3ExportsForLedger where
        toQuery ListJournalS3ExportsForLedger'{..}
          = mconcat
              ["next_token" =: _ljseflNextToken,
               "max_results" =: _ljseflMaxResults]

-- | /See:/ 'listJournalS3ExportsForLedgerResponse' smart constructor.
data ListJournalS3ExportsForLedgerResponse = ListJournalS3ExportsForLedgerResponse'
  { _ljseflrsJournalS3Exports :: !(Maybe [JournalS3ExportDescription])
  , _ljseflrsNextToken        :: !(Maybe Text)
  , _ljseflrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListJournalS3ExportsForLedgerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljseflrsJournalS3Exports' - Undocumented member.
--
-- * 'ljseflrsNextToken' - Undocumented member.
--
-- * 'ljseflrsResponseStatus' - -- | The response status code.
listJournalS3ExportsForLedgerResponse
    :: Int -- ^ 'ljseflrsResponseStatus'
    -> ListJournalS3ExportsForLedgerResponse
listJournalS3ExportsForLedgerResponse pResponseStatus_ =
  ListJournalS3ExportsForLedgerResponse'
    { _ljseflrsJournalS3Exports = Nothing
    , _ljseflrsNextToken = Nothing
    , _ljseflrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
ljseflrsJournalS3Exports :: Lens' ListJournalS3ExportsForLedgerResponse [JournalS3ExportDescription]
ljseflrsJournalS3Exports = lens _ljseflrsJournalS3Exports (\ s a -> s{_ljseflrsJournalS3Exports = a}) . _Default . _Coerce

-- | Undocumented member.
ljseflrsNextToken :: Lens' ListJournalS3ExportsForLedgerResponse (Maybe Text)
ljseflrsNextToken = lens _ljseflrsNextToken (\ s a -> s{_ljseflrsNextToken = a})

-- | -- | The response status code.
ljseflrsResponseStatus :: Lens' ListJournalS3ExportsForLedgerResponse Int
ljseflrsResponseStatus = lens _ljseflrsResponseStatus (\ s a -> s{_ljseflrsResponseStatus = a})

instance NFData ListJournalS3ExportsForLedgerResponse
         where
