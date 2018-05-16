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
-- Module      : Network.AWS.Greengrass.DisassociateServiceRoleFromAccount
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the service role from your account. Without a service role, deployments will not work.
module Network.AWS.Greengrass.DisassociateServiceRoleFromAccount
    (
    -- * Creating a Request
      disassociateServiceRoleFromAccount
    , DisassociateServiceRoleFromAccount

    -- * Destructuring the Response
    , disassociateServiceRoleFromAccountResponse
    , DisassociateServiceRoleFromAccountResponse
    -- * Response Lenses
    , dsrfarsDisassociatedAt
    , dsrfarsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateServiceRoleFromAccount' smart constructor.
data DisassociateServiceRoleFromAccount =
  DisassociateServiceRoleFromAccount'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateServiceRoleFromAccount' with the minimum fields required to make a request.
--
disassociateServiceRoleFromAccount
    :: DisassociateServiceRoleFromAccount
disassociateServiceRoleFromAccount = DisassociateServiceRoleFromAccount'


instance AWSRequest
           DisassociateServiceRoleFromAccount
         where
        type Rs DisassociateServiceRoleFromAccount =
             DisassociateServiceRoleFromAccountResponse
        request = delete greengrass
        response
          = receiveJSON
              (\ s h x ->
                 DisassociateServiceRoleFromAccountResponse' <$>
                   (x .?> "DisassociatedAt") <*> (pure (fromEnum s)))

instance Hashable DisassociateServiceRoleFromAccount
         where

instance NFData DisassociateServiceRoleFromAccount
         where

instance ToHeaders DisassociateServiceRoleFromAccount
         where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DisassociateServiceRoleFromAccount
         where
        toPath = const "/greengrass/servicerole"

instance ToQuery DisassociateServiceRoleFromAccount
         where
        toQuery = const mempty

-- | /See:/ 'disassociateServiceRoleFromAccountResponse' smart constructor.
data DisassociateServiceRoleFromAccountResponse = DisassociateServiceRoleFromAccountResponse'
  { _dsrfarsDisassociatedAt :: !(Maybe Text)
  , _dsrfarsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateServiceRoleFromAccountResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrfarsDisassociatedAt' - The time when the service role was disassociated from the account.
--
-- * 'dsrfarsResponseStatus' - -- | The response status code.
disassociateServiceRoleFromAccountResponse
    :: Int -- ^ 'dsrfarsResponseStatus'
    -> DisassociateServiceRoleFromAccountResponse
disassociateServiceRoleFromAccountResponse pResponseStatus_ =
  DisassociateServiceRoleFromAccountResponse'
    { _dsrfarsDisassociatedAt = Nothing
    , _dsrfarsResponseStatus = pResponseStatus_
    }


-- | The time when the service role was disassociated from the account.
dsrfarsDisassociatedAt :: Lens' DisassociateServiceRoleFromAccountResponse (Maybe Text)
dsrfarsDisassociatedAt = lens _dsrfarsDisassociatedAt (\ s a -> s{_dsrfarsDisassociatedAt = a})

-- | -- | The response status code.
dsrfarsResponseStatus :: Lens' DisassociateServiceRoleFromAccountResponse Int
dsrfarsResponseStatus = lens _dsrfarsResponseStatus (\ s a -> s{_dsrfarsResponseStatus = a})

instance NFData
           DisassociateServiceRoleFromAccountResponse
         where
