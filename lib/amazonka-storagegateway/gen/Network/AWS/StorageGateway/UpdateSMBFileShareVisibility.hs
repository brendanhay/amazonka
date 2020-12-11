{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.UpdateSMBFileShareVisibility
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Controls whether the shares on a gateway are visible in a net view or browse list.
module Network.AWS.StorageGateway.UpdateSMBFileShareVisibility
  ( -- * Creating a request
    UpdateSMBFileShareVisibility (..),
    mkUpdateSMBFileShareVisibility,

    -- ** Request lenses
    usmbfsvGatewayARN,
    usmbfsvFileSharesVisible,

    -- * Destructuring the response
    UpdateSMBFileShareVisibilityResponse (..),
    mkUpdateSMBFileShareVisibilityResponse,

    -- ** Response lenses
    usmbfsvrsGatewayARN,
    usmbfsvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | /See:/ 'mkUpdateSMBFileShareVisibility' smart constructor.
data UpdateSMBFileShareVisibility = UpdateSMBFileShareVisibility'
  { gatewayARN ::
      Lude.Text,
    fileSharesVisible :: Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSMBFileShareVisibility' with the minimum fields required to make a request.
--
-- * 'fileSharesVisible' - The shares on this gateway appear when listing shares.
-- * 'gatewayARN' - Undocumented field.
mkUpdateSMBFileShareVisibility ::
  -- | 'gatewayARN'
  Lude.Text ->
  -- | 'fileSharesVisible'
  Lude.Bool ->
  UpdateSMBFileShareVisibility
mkUpdateSMBFileShareVisibility pGatewayARN_ pFileSharesVisible_ =
  UpdateSMBFileShareVisibility'
    { gatewayARN = pGatewayARN_,
      fileSharesVisible = pFileSharesVisible_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsvGatewayARN :: Lens.Lens' UpdateSMBFileShareVisibility Lude.Text
usmbfsvGatewayARN = Lens.lens (gatewayARN :: UpdateSMBFileShareVisibility -> Lude.Text) (\s a -> s {gatewayARN = a} :: UpdateSMBFileShareVisibility)
{-# DEPRECATED usmbfsvGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The shares on this gateway appear when listing shares.
--
-- /Note:/ Consider using 'fileSharesVisible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsvFileSharesVisible :: Lens.Lens' UpdateSMBFileShareVisibility Lude.Bool
usmbfsvFileSharesVisible = Lens.lens (fileSharesVisible :: UpdateSMBFileShareVisibility -> Lude.Bool) (\s a -> s {fileSharesVisible = a} :: UpdateSMBFileShareVisibility)
{-# DEPRECATED usmbfsvFileSharesVisible "Use generic-lens or generic-optics with 'fileSharesVisible' instead." #-}

instance Lude.AWSRequest UpdateSMBFileShareVisibility where
  type
    Rs UpdateSMBFileShareVisibility =
      UpdateSMBFileShareVisibilityResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateSMBFileShareVisibilityResponse'
            Lude.<$> (x Lude..?> "GatewayARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateSMBFileShareVisibility where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.UpdateSMBFileShareVisibility" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateSMBFileShareVisibility where
  toJSON UpdateSMBFileShareVisibility' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("GatewayARN" Lude..= gatewayARN),
            Lude.Just ("FileSharesVisible" Lude..= fileSharesVisible)
          ]
      )

instance Lude.ToPath UpdateSMBFileShareVisibility where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateSMBFileShareVisibility where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateSMBFileShareVisibilityResponse' smart constructor.
data UpdateSMBFileShareVisibilityResponse = UpdateSMBFileShareVisibilityResponse'
  { gatewayARN ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSMBFileShareVisibilityResponse' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkUpdateSMBFileShareVisibilityResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateSMBFileShareVisibilityResponse
mkUpdateSMBFileShareVisibilityResponse pResponseStatus_ =
  UpdateSMBFileShareVisibilityResponse'
    { gatewayARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsvrsGatewayARN :: Lens.Lens' UpdateSMBFileShareVisibilityResponse (Lude.Maybe Lude.Text)
usmbfsvrsGatewayARN = Lens.lens (gatewayARN :: UpdateSMBFileShareVisibilityResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: UpdateSMBFileShareVisibilityResponse)
{-# DEPRECATED usmbfsvrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsvrsResponseStatus :: Lens.Lens' UpdateSMBFileShareVisibilityResponse Lude.Int
usmbfsvrsResponseStatus = Lens.lens (responseStatus :: UpdateSMBFileShareVisibilityResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateSMBFileShareVisibilityResponse)
{-# DEPRECATED usmbfsvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
