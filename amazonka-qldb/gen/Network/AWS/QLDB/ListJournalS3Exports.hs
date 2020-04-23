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
-- Module      : Network.AWS.QLDB.ListJournalS3Exports
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
--
-- This operation returns paginated results.
module Network.AWS.QLDB.ListJournalS3Exports
    (
    -- * Creating a Request
      listJournalS3Exports
    , ListJournalS3Exports
    -- * Request Lenses
    , ljseNextToken
    , ljseMaxResults

    -- * Destructuring the Response
    , listJournalS3ExportsResponse
    , ListJournalS3ExportsResponse
    -- * Response Lenses
    , ljsersJournalS3Exports
    , ljsersNextToken
    , ljsersResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.QLDB.Types
import Network.AWS.QLDB.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listJournalS3Exports' smart constructor.
data ListJournalS3Exports = ListJournalS3Exports'
  { _ljseNextToken  :: !(Maybe Text)
  , _ljseMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListJournalS3Exports' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljseNextToken' - Undocumented member.
--
-- * 'ljseMaxResults' - Undocumented member.
listJournalS3Exports
    :: ListJournalS3Exports
listJournalS3Exports =
  ListJournalS3Exports' {_ljseNextToken = Nothing, _ljseMaxResults = Nothing}


-- | Undocumented member.
ljseNextToken :: Lens' ListJournalS3Exports (Maybe Text)
ljseNextToken = lens _ljseNextToken (\ s a -> s{_ljseNextToken = a})

-- | Undocumented member.
ljseMaxResults :: Lens' ListJournalS3Exports (Maybe Natural)
ljseMaxResults = lens _ljseMaxResults (\ s a -> s{_ljseMaxResults = a}) . mapping _Nat

instance AWSPager ListJournalS3Exports where
        page rq rs
          | stop (rs ^. ljsersNextToken) = Nothing
          | otherwise =
            Just $ rq & ljseNextToken .~ rs ^. ljsersNextToken

instance AWSRequest ListJournalS3Exports where
        type Rs ListJournalS3Exports =
             ListJournalS3ExportsResponse
        request = get qldb
        response
          = receiveJSON
              (\ s h x ->
                 ListJournalS3ExportsResponse' <$>
                   (x .?> "JournalS3Exports" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListJournalS3Exports where

instance NFData ListJournalS3Exports where

instance ToHeaders ListJournalS3Exports where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToPath ListJournalS3Exports where
        toPath = const "/journal-s3-exports"

instance ToQuery ListJournalS3Exports where
        toQuery ListJournalS3Exports'{..}
          = mconcat
              ["next_token" =: _ljseNextToken,
               "max_results" =: _ljseMaxResults]

-- | /See:/ 'listJournalS3ExportsResponse' smart constructor.
data ListJournalS3ExportsResponse = ListJournalS3ExportsResponse'
  { _ljsersJournalS3Exports :: !(Maybe [JournalS3ExportDescription])
  , _ljsersNextToken        :: !(Maybe Text)
  , _ljsersResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListJournalS3ExportsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljsersJournalS3Exports' - Undocumented member.
--
-- * 'ljsersNextToken' - Undocumented member.
--
-- * 'ljsersResponseStatus' - -- | The response status code.
listJournalS3ExportsResponse
    :: Int -- ^ 'ljsersResponseStatus'
    -> ListJournalS3ExportsResponse
listJournalS3ExportsResponse pResponseStatus_ =
  ListJournalS3ExportsResponse'
    { _ljsersJournalS3Exports = Nothing
    , _ljsersNextToken = Nothing
    , _ljsersResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
ljsersJournalS3Exports :: Lens' ListJournalS3ExportsResponse [JournalS3ExportDescription]
ljsersJournalS3Exports = lens _ljsersJournalS3Exports (\ s a -> s{_ljsersJournalS3Exports = a}) . _Default . _Coerce

-- | Undocumented member.
ljsersNextToken :: Lens' ListJournalS3ExportsResponse (Maybe Text)
ljsersNextToken = lens _ljsersNextToken (\ s a -> s{_ljsersNextToken = a})

-- | -- | The response status code.
ljsersResponseStatus :: Lens' ListJournalS3ExportsResponse Int
ljsersResponseStatus = lens _ljsersResponseStatus (\ s a -> s{_ljsersResponseStatus = a})

instance NFData ListJournalS3ExportsResponse where
