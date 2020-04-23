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
-- Module      : Network.AWS.QLDB.DeleteLedger
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.QLDB.DeleteLedger
    (
    -- * Creating a Request
      deleteLedger
    , DeleteLedger
    -- * Request Lenses
    , dlName

    -- * Destructuring the Response
    , deleteLedgerResponse
    , DeleteLedgerResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.QLDB.Types
import Network.AWS.QLDB.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteLedger' smart constructor.
newtype DeleteLedger = DeleteLedger'
  { _dlName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLedger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlName' - Undocumented member.
deleteLedger
    :: Text -- ^ 'dlName'
    -> DeleteLedger
deleteLedger pName_ = DeleteLedger' {_dlName = pName_}


-- | Undocumented member.
dlName :: Lens' DeleteLedger Text
dlName = lens _dlName (\ s a -> s{_dlName = a})

instance AWSRequest DeleteLedger where
        type Rs DeleteLedger = DeleteLedgerResponse
        request = delete qldb
        response = receiveNull DeleteLedgerResponse'

instance Hashable DeleteLedger where

instance NFData DeleteLedger where

instance ToHeaders DeleteLedger where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToPath DeleteLedger where
        toPath DeleteLedger'{..}
          = mconcat ["/ledgers/", toBS _dlName]

instance ToQuery DeleteLedger where
        toQuery = const mempty

-- | /See:/ 'deleteLedgerResponse' smart constructor.
data DeleteLedgerResponse =
  DeleteLedgerResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLedgerResponse' with the minimum fields required to make a request.
--
deleteLedgerResponse
    :: DeleteLedgerResponse
deleteLedgerResponse = DeleteLedgerResponse'


instance NFData DeleteLedgerResponse where
