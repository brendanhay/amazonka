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
-- Module      : Network.AWS.GuardDuty.DisassociateFromMasterAccount
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the current GuardDuty member account from its master account.
module Network.AWS.GuardDuty.DisassociateFromMasterAccount
    (
    -- * Creating a Request
      disassociateFromMasterAccount
    , DisassociateFromMasterAccount
    -- * Request Lenses
    , dfmaDetectorId

    -- * Destructuring the Response
    , disassociateFromMasterAccountResponse
    , DisassociateFromMasterAccountResponse
    -- * Response Lenses
    , dfmarsResponseStatus
    ) where

import Network.AWS.GuardDuty.Types
import Network.AWS.GuardDuty.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateFromMasterAccount' smart constructor.
newtype DisassociateFromMasterAccount = DisassociateFromMasterAccount'
  { _dfmaDetectorId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateFromMasterAccount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfmaDetectorId' - The unique ID of the detector of the GuardDuty member account.
disassociateFromMasterAccount
    :: Text -- ^ 'dfmaDetectorId'
    -> DisassociateFromMasterAccount
disassociateFromMasterAccount pDetectorId_ =
  DisassociateFromMasterAccount' {_dfmaDetectorId = pDetectorId_}


-- | The unique ID of the detector of the GuardDuty member account.
dfmaDetectorId :: Lens' DisassociateFromMasterAccount Text
dfmaDetectorId = lens _dfmaDetectorId (\ s a -> s{_dfmaDetectorId = a})

instance AWSRequest DisassociateFromMasterAccount
         where
        type Rs DisassociateFromMasterAccount =
             DisassociateFromMasterAccountResponse
        request = postJSON guardDuty
        response
          = receiveEmpty
              (\ s h x ->
                 DisassociateFromMasterAccountResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DisassociateFromMasterAccount where

instance NFData DisassociateFromMasterAccount where

instance ToHeaders DisassociateFromMasterAccount
         where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisassociateFromMasterAccount where
        toJSON = const (Object mempty)

instance ToPath DisassociateFromMasterAccount where
        toPath DisassociateFromMasterAccount'{..}
          = mconcat
              ["/detector/", toBS _dfmaDetectorId,
               "/master/disassociate"]

instance ToQuery DisassociateFromMasterAccount where
        toQuery = const mempty

-- | /See:/ 'disassociateFromMasterAccountResponse' smart constructor.
newtype DisassociateFromMasterAccountResponse = DisassociateFromMasterAccountResponse'
  { _dfmarsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateFromMasterAccountResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfmarsResponseStatus' - -- | The response status code.
disassociateFromMasterAccountResponse
    :: Int -- ^ 'dfmarsResponseStatus'
    -> DisassociateFromMasterAccountResponse
disassociateFromMasterAccountResponse pResponseStatus_ =
  DisassociateFromMasterAccountResponse'
    {_dfmarsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dfmarsResponseStatus :: Lens' DisassociateFromMasterAccountResponse Int
dfmarsResponseStatus = lens _dfmarsResponseStatus (\ s a -> s{_dfmarsResponseStatus = a})

instance NFData DisassociateFromMasterAccountResponse
         where
