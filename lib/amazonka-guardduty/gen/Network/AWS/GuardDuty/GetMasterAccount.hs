{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.GetMasterAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides the details for the GuardDuty master account associated with the current GuardDuty member account.
module Network.AWS.GuardDuty.GetMasterAccount
  ( -- * Creating a request
    GetMasterAccount (..),
    mkGetMasterAccount,

    -- ** Request lenses
    gmaDetectorId,

    -- * Destructuring the response
    GetMasterAccountResponse (..),
    mkGetMasterAccountResponse,

    -- ** Response lenses
    gmarsMaster,
    gmarsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetMasterAccount' smart constructor.
newtype GetMasterAccount = GetMasterAccount'
  { -- | The unique ID of the detector of the GuardDuty member account.
    detectorId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMasterAccount' with the minimum fields required to make a request.
--
-- * 'detectorId' - The unique ID of the detector of the GuardDuty member account.
mkGetMasterAccount ::
  -- | 'detectorId'
  Lude.Text ->
  GetMasterAccount
mkGetMasterAccount pDetectorId_ =
  GetMasterAccount' {detectorId = pDetectorId_}

-- | The unique ID of the detector of the GuardDuty member account.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmaDetectorId :: Lens.Lens' GetMasterAccount Lude.Text
gmaDetectorId = Lens.lens (detectorId :: GetMasterAccount -> Lude.Text) (\s a -> s {detectorId = a} :: GetMasterAccount)
{-# DEPRECATED gmaDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

instance Lude.AWSRequest GetMasterAccount where
  type Rs GetMasterAccount = GetMasterAccountResponse
  request = Req.get guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetMasterAccountResponse'
            Lude.<$> (x Lude..:> "master") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetMasterAccount where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetMasterAccount where
  toPath GetMasterAccount' {..} =
    Lude.mconcat ["/detector/", Lude.toBS detectorId, "/master"]

instance Lude.ToQuery GetMasterAccount where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetMasterAccountResponse' smart constructor.
data GetMasterAccountResponse = GetMasterAccountResponse'
  { -- | The master account details.
    master :: Master,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMasterAccountResponse' with the minimum fields required to make a request.
--
-- * 'master' - The master account details.
-- * 'responseStatus' - The response status code.
mkGetMasterAccountResponse ::
  -- | 'master'
  Master ->
  -- | 'responseStatus'
  Lude.Int ->
  GetMasterAccountResponse
mkGetMasterAccountResponse pMaster_ pResponseStatus_ =
  GetMasterAccountResponse'
    { master = pMaster_,
      responseStatus = pResponseStatus_
    }

-- | The master account details.
--
-- /Note:/ Consider using 'master' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmarsMaster :: Lens.Lens' GetMasterAccountResponse Master
gmarsMaster = Lens.lens (master :: GetMasterAccountResponse -> Master) (\s a -> s {master = a} :: GetMasterAccountResponse)
{-# DEPRECATED gmarsMaster "Use generic-lens or generic-optics with 'master' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmarsResponseStatus :: Lens.Lens' GetMasterAccountResponse Lude.Int
gmarsResponseStatus = Lens.lens (responseStatus :: GetMasterAccountResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetMasterAccountResponse)
{-# DEPRECATED gmarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
