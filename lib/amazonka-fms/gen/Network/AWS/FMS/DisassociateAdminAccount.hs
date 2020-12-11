{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.DisassociateAdminAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the account that has been set as the AWS Firewall Manager administrator account. To set a different account as the administrator account, you must submit an @AssociateAdminAccount@ request.
module Network.AWS.FMS.DisassociateAdminAccount
  ( -- * Creating a request
    DisassociateAdminAccount (..),
    mkDisassociateAdminAccount,

    -- * Destructuring the response
    DisassociateAdminAccountResponse (..),
    mkDisassociateAdminAccountResponse,
  )
where

import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateAdminAccount' smart constructor.
data DisassociateAdminAccount = DisassociateAdminAccount'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateAdminAccount' with the minimum fields required to make a request.
mkDisassociateAdminAccount ::
  DisassociateAdminAccount
mkDisassociateAdminAccount = DisassociateAdminAccount'

instance Lude.AWSRequest DisassociateAdminAccount where
  type Rs DisassociateAdminAccount = DisassociateAdminAccountResponse
  request = Req.postJSON fmsService
  response = Res.receiveNull DisassociateAdminAccountResponse'

instance Lude.ToHeaders DisassociateAdminAccount where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSFMS_20180101.DisassociateAdminAccount" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisassociateAdminAccount where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DisassociateAdminAccount where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateAdminAccount where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateAdminAccountResponse' smart constructor.
data DisassociateAdminAccountResponse = DisassociateAdminAccountResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateAdminAccountResponse' with the minimum fields required to make a request.
mkDisassociateAdminAccountResponse ::
  DisassociateAdminAccountResponse
mkDisassociateAdminAccountResponse =
  DisassociateAdminAccountResponse'
