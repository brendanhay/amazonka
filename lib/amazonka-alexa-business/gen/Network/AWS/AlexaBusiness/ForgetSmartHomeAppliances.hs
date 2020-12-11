{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.ForgetSmartHomeAppliances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Forgets smart home appliances associated to a room.
module Network.AWS.AlexaBusiness.ForgetSmartHomeAppliances
  ( -- * Creating a request
    ForgetSmartHomeAppliances (..),
    mkForgetSmartHomeAppliances,

    -- ** Request lenses
    fshaRoomARN,

    -- * Destructuring the response
    ForgetSmartHomeAppliancesResponse (..),
    mkForgetSmartHomeAppliancesResponse,

    -- ** Response lenses
    fsharsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkForgetSmartHomeAppliances' smart constructor.
newtype ForgetSmartHomeAppliances = ForgetSmartHomeAppliances'
  { roomARN ::
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

-- | Creates a value of 'ForgetSmartHomeAppliances' with the minimum fields required to make a request.
--
-- * 'roomARN' - The room that the appliances are associated with.
mkForgetSmartHomeAppliances ::
  -- | 'roomARN'
  Lude.Text ->
  ForgetSmartHomeAppliances
mkForgetSmartHomeAppliances pRoomARN_ =
  ForgetSmartHomeAppliances' {roomARN = pRoomARN_}

-- | The room that the appliances are associated with.
--
-- /Note:/ Consider using 'roomARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fshaRoomARN :: Lens.Lens' ForgetSmartHomeAppliances Lude.Text
fshaRoomARN = Lens.lens (roomARN :: ForgetSmartHomeAppliances -> Lude.Text) (\s a -> s {roomARN = a} :: ForgetSmartHomeAppliances)
{-# DEPRECATED fshaRoomARN "Use generic-lens or generic-optics with 'roomARN' instead." #-}

instance Lude.AWSRequest ForgetSmartHomeAppliances where
  type
    Rs ForgetSmartHomeAppliances =
      ForgetSmartHomeAppliancesResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          ForgetSmartHomeAppliancesResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ForgetSmartHomeAppliances where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.ForgetSmartHomeAppliances" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ForgetSmartHomeAppliances where
  toJSON ForgetSmartHomeAppliances' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("RoomArn" Lude..= roomARN)])

instance Lude.ToPath ForgetSmartHomeAppliances where
  toPath = Lude.const "/"

instance Lude.ToQuery ForgetSmartHomeAppliances where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkForgetSmartHomeAppliancesResponse' smart constructor.
newtype ForgetSmartHomeAppliancesResponse = ForgetSmartHomeAppliancesResponse'
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

-- | Creates a value of 'ForgetSmartHomeAppliancesResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkForgetSmartHomeAppliancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ForgetSmartHomeAppliancesResponse
mkForgetSmartHomeAppliancesResponse pResponseStatus_ =
  ForgetSmartHomeAppliancesResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsharsResponseStatus :: Lens.Lens' ForgetSmartHomeAppliancesResponse Lude.Int
fsharsResponseStatus = Lens.lens (responseStatus :: ForgetSmartHomeAppliancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ForgetSmartHomeAppliancesResponse)
{-# DEPRECATED fsharsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
