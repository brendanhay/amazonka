{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.UpdateHITTypeOfHIT
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @UpdateHITTypeOfHIT@ operation allows you to change the HITType properties of a HIT. This operation disassociates the HIT from its old HITType properties and associates it with the new HITType properties. The HIT takes on the properties of the new HITType in place of the old ones.
module Network.AWS.MechanicalTurk.UpdateHITTypeOfHIT
  ( -- * Creating a request
    UpdateHITTypeOfHIT (..),
    mkUpdateHITTypeOfHIT,

    -- ** Request lenses
    uhittohitHITTypeId,
    uhittohitHITId,

    -- * Destructuring the response
    UpdateHITTypeOfHITResponse (..),
    mkUpdateHITTypeOfHITResponse,

    -- ** Response lenses
    uhittohitrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateHITTypeOfHIT' smart constructor.
data UpdateHITTypeOfHIT = UpdateHITTypeOfHIT'
  { -- | The ID of the new HIT type.
    hITTypeId :: Lude.Text,
    -- | The HIT to update.
    hITId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateHITTypeOfHIT' with the minimum fields required to make a request.
--
-- * 'hITTypeId' - The ID of the new HIT type.
-- * 'hITId' - The HIT to update.
mkUpdateHITTypeOfHIT ::
  -- | 'hITTypeId'
  Lude.Text ->
  -- | 'hITId'
  Lude.Text ->
  UpdateHITTypeOfHIT
mkUpdateHITTypeOfHIT pHITTypeId_ pHITId_ =
  UpdateHITTypeOfHIT' {hITTypeId = pHITTypeId_, hITId = pHITId_}

-- | The ID of the new HIT type.
--
-- /Note:/ Consider using 'hITTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhittohitHITTypeId :: Lens.Lens' UpdateHITTypeOfHIT Lude.Text
uhittohitHITTypeId = Lens.lens (hITTypeId :: UpdateHITTypeOfHIT -> Lude.Text) (\s a -> s {hITTypeId = a} :: UpdateHITTypeOfHIT)
{-# DEPRECATED uhittohitHITTypeId "Use generic-lens or generic-optics with 'hITTypeId' instead." #-}

-- | The HIT to update.
--
-- /Note:/ Consider using 'hITId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhittohitHITId :: Lens.Lens' UpdateHITTypeOfHIT Lude.Text
uhittohitHITId = Lens.lens (hITId :: UpdateHITTypeOfHIT -> Lude.Text) (\s a -> s {hITId = a} :: UpdateHITTypeOfHIT)
{-# DEPRECATED uhittohitHITId "Use generic-lens or generic-optics with 'hITId' instead." #-}

instance Lude.AWSRequest UpdateHITTypeOfHIT where
  type Rs UpdateHITTypeOfHIT = UpdateHITTypeOfHITResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateHITTypeOfHITResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateHITTypeOfHIT where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MTurkRequesterServiceV20170117.UpdateHITTypeOfHIT" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateHITTypeOfHIT where
  toJSON UpdateHITTypeOfHIT' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("HITTypeId" Lude..= hITTypeId),
            Lude.Just ("HITId" Lude..= hITId)
          ]
      )

instance Lude.ToPath UpdateHITTypeOfHIT where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateHITTypeOfHIT where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateHITTypeOfHITResponse' smart constructor.
newtype UpdateHITTypeOfHITResponse = UpdateHITTypeOfHITResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateHITTypeOfHITResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateHITTypeOfHITResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateHITTypeOfHITResponse
mkUpdateHITTypeOfHITResponse pResponseStatus_ =
  UpdateHITTypeOfHITResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhittohitrsResponseStatus :: Lens.Lens' UpdateHITTypeOfHITResponse Lude.Int
uhittohitrsResponseStatus = Lens.lens (responseStatus :: UpdateHITTypeOfHITResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateHITTypeOfHITResponse)
{-# DEPRECATED uhittohitrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
