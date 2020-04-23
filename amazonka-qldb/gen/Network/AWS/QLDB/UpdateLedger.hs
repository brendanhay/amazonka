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
-- Module      : Network.AWS.QLDB.UpdateLedger
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.QLDB.UpdateLedger
    (
    -- * Creating a Request
      updateLedger
    , UpdateLedger
    -- * Request Lenses
    , ulDeletionProtection
    , ulName

    -- * Destructuring the Response
    , updateLedgerResponse
    , UpdateLedgerResponse
    -- * Response Lenses
    , ulrsState
    , ulrsDeletionProtection
    , ulrsARN
    , ulrsName
    , ulrsCreationDateTime
    , ulrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.QLDB.Types
import Network.AWS.QLDB.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateLedger' smart constructor.
data UpdateLedger = UpdateLedger'
  { _ulDeletionProtection :: !(Maybe Bool)
  , _ulName               :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateLedger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ulDeletionProtection' - Undocumented member.
--
-- * 'ulName' - Undocumented member.
updateLedger
    :: Text -- ^ 'ulName'
    -> UpdateLedger
updateLedger pName_ =
  UpdateLedger' {_ulDeletionProtection = Nothing, _ulName = pName_}


-- | Undocumented member.
ulDeletionProtection :: Lens' UpdateLedger (Maybe Bool)
ulDeletionProtection = lens _ulDeletionProtection (\ s a -> s{_ulDeletionProtection = a})

-- | Undocumented member.
ulName :: Lens' UpdateLedger Text
ulName = lens _ulName (\ s a -> s{_ulName = a})

instance AWSRequest UpdateLedger where
        type Rs UpdateLedger = UpdateLedgerResponse
        request = patchJSON qldb
        response
          = receiveJSON
              (\ s h x ->
                 UpdateLedgerResponse' <$>
                   (x .?> "State") <*> (x .?> "DeletionProtection") <*>
                     (x .?> "Arn")
                     <*> (x .?> "Name")
                     <*> (x .?> "CreationDateTime")
                     <*> (pure (fromEnum s)))

instance Hashable UpdateLedger where

instance NFData UpdateLedger where

instance ToHeaders UpdateLedger where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON UpdateLedger where
        toJSON UpdateLedger'{..}
          = object
              (catMaybes
                 [("DeletionProtection" .=) <$>
                    _ulDeletionProtection])

instance ToPath UpdateLedger where
        toPath UpdateLedger'{..}
          = mconcat ["/ledgers/", toBS _ulName]

instance ToQuery UpdateLedger where
        toQuery = const mempty

-- | /See:/ 'updateLedgerResponse' smart constructor.
data UpdateLedgerResponse = UpdateLedgerResponse'
  { _ulrsState              :: !(Maybe LedgerState)
  , _ulrsDeletionProtection :: !(Maybe Bool)
  , _ulrsARN                :: !(Maybe Text)
  , _ulrsName               :: !(Maybe Text)
  , _ulrsCreationDateTime   :: !(Maybe POSIX)
  , _ulrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateLedgerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ulrsState' - Undocumented member.
--
-- * 'ulrsDeletionProtection' - Undocumented member.
--
-- * 'ulrsARN' - Undocumented member.
--
-- * 'ulrsName' - Undocumented member.
--
-- * 'ulrsCreationDateTime' - Undocumented member.
--
-- * 'ulrsResponseStatus' - -- | The response status code.
updateLedgerResponse
    :: Int -- ^ 'ulrsResponseStatus'
    -> UpdateLedgerResponse
updateLedgerResponse pResponseStatus_ =
  UpdateLedgerResponse'
    { _ulrsState = Nothing
    , _ulrsDeletionProtection = Nothing
    , _ulrsARN = Nothing
    , _ulrsName = Nothing
    , _ulrsCreationDateTime = Nothing
    , _ulrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
ulrsState :: Lens' UpdateLedgerResponse (Maybe LedgerState)
ulrsState = lens _ulrsState (\ s a -> s{_ulrsState = a})

-- | Undocumented member.
ulrsDeletionProtection :: Lens' UpdateLedgerResponse (Maybe Bool)
ulrsDeletionProtection = lens _ulrsDeletionProtection (\ s a -> s{_ulrsDeletionProtection = a})

-- | Undocumented member.
ulrsARN :: Lens' UpdateLedgerResponse (Maybe Text)
ulrsARN = lens _ulrsARN (\ s a -> s{_ulrsARN = a})

-- | Undocumented member.
ulrsName :: Lens' UpdateLedgerResponse (Maybe Text)
ulrsName = lens _ulrsName (\ s a -> s{_ulrsName = a})

-- | Undocumented member.
ulrsCreationDateTime :: Lens' UpdateLedgerResponse (Maybe UTCTime)
ulrsCreationDateTime = lens _ulrsCreationDateTime (\ s a -> s{_ulrsCreationDateTime = a}) . mapping _Time

-- | -- | The response status code.
ulrsResponseStatus :: Lens' UpdateLedgerResponse Int
ulrsResponseStatus = lens _ulrsResponseStatus (\ s a -> s{_ulrsResponseStatus = a})

instance NFData UpdateLedgerResponse where
