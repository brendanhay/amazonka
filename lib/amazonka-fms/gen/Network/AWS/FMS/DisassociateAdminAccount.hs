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
-- Module      : Network.AWS.FMS.DisassociateAdminAccount
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the account that has been set as the AWS Firewall Manager administrator account. You will need to submit an @AssociateAdminAccount@ request to set a new account as the AWS Firewall administrator.
--
--
module Network.AWS.FMS.DisassociateAdminAccount
    (
    -- * Creating a Request
      disassociateAdminAccount
    , DisassociateAdminAccount

    -- * Destructuring the Response
    , disassociateAdminAccountResponse
    , DisassociateAdminAccountResponse
    ) where

import Network.AWS.FMS.Types
import Network.AWS.FMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateAdminAccount' smart constructor.
data DisassociateAdminAccount =
  DisassociateAdminAccount'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateAdminAccount' with the minimum fields required to make a request.
--
disassociateAdminAccount
    :: DisassociateAdminAccount
disassociateAdminAccount = DisassociateAdminAccount'


instance AWSRequest DisassociateAdminAccount where
        type Rs DisassociateAdminAccount =
             DisassociateAdminAccountResponse
        request = postJSON fms
        response
          = receiveNull DisassociateAdminAccountResponse'

instance Hashable DisassociateAdminAccount where

instance NFData DisassociateAdminAccount where

instance ToHeaders DisassociateAdminAccount where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSFMS_20180101.DisassociateAdminAccount" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisassociateAdminAccount where
        toJSON = const (Object mempty)

instance ToPath DisassociateAdminAccount where
        toPath = const "/"

instance ToQuery DisassociateAdminAccount where
        toQuery = const mempty

-- | /See:/ 'disassociateAdminAccountResponse' smart constructor.
data DisassociateAdminAccountResponse =
  DisassociateAdminAccountResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateAdminAccountResponse' with the minimum fields required to make a request.
--
disassociateAdminAccountResponse
    :: DisassociateAdminAccountResponse
disassociateAdminAccountResponse = DisassociateAdminAccountResponse'


instance NFData DisassociateAdminAccountResponse
         where
