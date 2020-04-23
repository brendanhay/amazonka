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
-- Module      : Network.AWS.QLDB.DescribeLedger
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.QLDB.DescribeLedger
    (
    -- * Creating a Request
      describeLedger
    , DescribeLedger
    -- * Request Lenses
    , dName

    -- * Destructuring the Response
    , describeLedgerResponse
    , DescribeLedgerResponse
    -- * Response Lenses
    , dlrsState
    , dlrsDeletionProtection
    , dlrsARN
    , dlrsName
    , dlrsCreationDateTime
    , dlrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.QLDB.Types
import Network.AWS.QLDB.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeLedger' smart constructor.
newtype DescribeLedger = DescribeLedger'
  { _dName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLedger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dName' - Undocumented member.
describeLedger
    :: Text -- ^ 'dName'
    -> DescribeLedger
describeLedger pName_ = DescribeLedger' {_dName = pName_}


-- | Undocumented member.
dName :: Lens' DescribeLedger Text
dName = lens _dName (\ s a -> s{_dName = a})

instance AWSRequest DescribeLedger where
        type Rs DescribeLedger = DescribeLedgerResponse
        request = get qldb
        response
          = receiveJSON
              (\ s h x ->
                 DescribeLedgerResponse' <$>
                   (x .?> "State") <*> (x .?> "DeletionProtection") <*>
                     (x .?> "Arn")
                     <*> (x .?> "Name")
                     <*> (x .?> "CreationDateTime")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeLedger where

instance NFData DescribeLedger where

instance ToHeaders DescribeLedger where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToPath DescribeLedger where
        toPath DescribeLedger'{..}
          = mconcat ["/ledgers/", toBS _dName]

instance ToQuery DescribeLedger where
        toQuery = const mempty

-- | /See:/ 'describeLedgerResponse' smart constructor.
data DescribeLedgerResponse = DescribeLedgerResponse'
  { _dlrsState              :: !(Maybe LedgerState)
  , _dlrsDeletionProtection :: !(Maybe Bool)
  , _dlrsARN                :: !(Maybe Text)
  , _dlrsName               :: !(Maybe Text)
  , _dlrsCreationDateTime   :: !(Maybe POSIX)
  , _dlrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLedgerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlrsState' - Undocumented member.
--
-- * 'dlrsDeletionProtection' - Undocumented member.
--
-- * 'dlrsARN' - Undocumented member.
--
-- * 'dlrsName' - Undocumented member.
--
-- * 'dlrsCreationDateTime' - Undocumented member.
--
-- * 'dlrsResponseStatus' - -- | The response status code.
describeLedgerResponse
    :: Int -- ^ 'dlrsResponseStatus'
    -> DescribeLedgerResponse
describeLedgerResponse pResponseStatus_ =
  DescribeLedgerResponse'
    { _dlrsState = Nothing
    , _dlrsDeletionProtection = Nothing
    , _dlrsARN = Nothing
    , _dlrsName = Nothing
    , _dlrsCreationDateTime = Nothing
    , _dlrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
dlrsState :: Lens' DescribeLedgerResponse (Maybe LedgerState)
dlrsState = lens _dlrsState (\ s a -> s{_dlrsState = a})

-- | Undocumented member.
dlrsDeletionProtection :: Lens' DescribeLedgerResponse (Maybe Bool)
dlrsDeletionProtection = lens _dlrsDeletionProtection (\ s a -> s{_dlrsDeletionProtection = a})

-- | Undocumented member.
dlrsARN :: Lens' DescribeLedgerResponse (Maybe Text)
dlrsARN = lens _dlrsARN (\ s a -> s{_dlrsARN = a})

-- | Undocumented member.
dlrsName :: Lens' DescribeLedgerResponse (Maybe Text)
dlrsName = lens _dlrsName (\ s a -> s{_dlrsName = a})

-- | Undocumented member.
dlrsCreationDateTime :: Lens' DescribeLedgerResponse (Maybe UTCTime)
dlrsCreationDateTime = lens _dlrsCreationDateTime (\ s a -> s{_dlrsCreationDateTime = a}) . mapping _Time

-- | -- | The response status code.
dlrsResponseStatus :: Lens' DescribeLedgerResponse Int
dlrsResponseStatus = lens _dlrsResponseStatus (\ s a -> s{_dlrsResponseStatus = a})

instance NFData DescribeLedgerResponse where
