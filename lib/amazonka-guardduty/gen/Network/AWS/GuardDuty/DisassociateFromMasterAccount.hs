{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.DisassociateFromMasterAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the current GuardDuty member account from its master account.
module Network.AWS.GuardDuty.DisassociateFromMasterAccount
  ( -- * Creating a request
    DisassociateFromMasterAccount (..),
    mkDisassociateFromMasterAccount,

    -- ** Request lenses
    dfmaDetectorId,

    -- * Destructuring the response
    DisassociateFromMasterAccountResponse (..),
    mkDisassociateFromMasterAccountResponse,

    -- ** Response lenses
    dfmarsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateFromMasterAccount' smart constructor.
newtype DisassociateFromMasterAccount = DisassociateFromMasterAccount'
  { detectorId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateFromMasterAccount' with the minimum fields required to make a request.
--
-- * 'detectorId' - The unique ID of the detector of the GuardDuty member account.
mkDisassociateFromMasterAccount ::
  -- | 'detectorId'
  Lude.Text ->
  DisassociateFromMasterAccount
mkDisassociateFromMasterAccount pDetectorId_ =
  DisassociateFromMasterAccount' {detectorId = pDetectorId_}

-- | The unique ID of the detector of the GuardDuty member account.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfmaDetectorId :: Lens.Lens' DisassociateFromMasterAccount Lude.Text
dfmaDetectorId = Lens.lens (detectorId :: DisassociateFromMasterAccount -> Lude.Text) (\s a -> s {detectorId = a} :: DisassociateFromMasterAccount)
{-# DEPRECATED dfmaDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

instance Lude.AWSRequest DisassociateFromMasterAccount where
  type
    Rs DisassociateFromMasterAccount =
      DisassociateFromMasterAccountResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisassociateFromMasterAccountResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateFromMasterAccount where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisassociateFromMasterAccount where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DisassociateFromMasterAccount where
  toPath DisassociateFromMasterAccount' {..} =
    Lude.mconcat
      ["/detector/", Lude.toBS detectorId, "/master/disassociate"]

instance Lude.ToQuery DisassociateFromMasterAccount where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateFromMasterAccountResponse' smart constructor.
newtype DisassociateFromMasterAccountResponse = DisassociateFromMasterAccountResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateFromMasterAccountResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisassociateFromMasterAccountResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateFromMasterAccountResponse
mkDisassociateFromMasterAccountResponse pResponseStatus_ =
  DisassociateFromMasterAccountResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfmarsResponseStatus :: Lens.Lens' DisassociateFromMasterAccountResponse Lude.Int
dfmarsResponseStatus = Lens.lens (responseStatus :: DisassociateFromMasterAccountResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateFromMasterAccountResponse)
{-# DEPRECATED dfmarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
