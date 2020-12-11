{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.UpdateSMBSecurityStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the SMB security strategy on a file gateway. This action is only supported in file gateways.
module Network.AWS.StorageGateway.UpdateSMBSecurityStrategy
  ( -- * Creating a request
    UpdateSMBSecurityStrategy (..),
    mkUpdateSMBSecurityStrategy,

    -- ** Request lenses
    usmbssGatewayARN,
    usmbssSMBSecurityStrategy,

    -- * Destructuring the response
    UpdateSMBSecurityStrategyResponse (..),
    mkUpdateSMBSecurityStrategyResponse,

    -- ** Response lenses
    usmbssrsGatewayARN,
    usmbssrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | /See:/ 'mkUpdateSMBSecurityStrategy' smart constructor.
data UpdateSMBSecurityStrategy = UpdateSMBSecurityStrategy'
  { gatewayARN ::
      Lude.Text,
    sMBSecurityStrategy ::
      SMBSecurityStrategy
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSMBSecurityStrategy' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - Undocumented field.
-- * 'sMBSecurityStrategy' - Specifies the type of security strategy.
--
-- ClientSpecified: if you use this option, requests are established based on what is negotiated by the client. This option is recommended when you want to maximize compatibility across different clients in your environment.
-- MandatorySigning: if you use this option, file gateway only allows connections from SMBv2 or SMBv3 clients that have signing enabled. This option works with SMB clients on Microsoft Windows Vista, Windows Server 2008 or newer.
-- MandatoryEncryption: if you use this option, file gateway only allows connections from SMBv3 clients that have encryption enabled. This option is highly recommended for environments that handle sensitive data. This option works with SMB clients on Microsoft Windows 8, Windows Server 2012 or newer.
mkUpdateSMBSecurityStrategy ::
  -- | 'gatewayARN'
  Lude.Text ->
  -- | 'sMBSecurityStrategy'
  SMBSecurityStrategy ->
  UpdateSMBSecurityStrategy
mkUpdateSMBSecurityStrategy pGatewayARN_ pSMBSecurityStrategy_ =
  UpdateSMBSecurityStrategy'
    { gatewayARN = pGatewayARN_,
      sMBSecurityStrategy = pSMBSecurityStrategy_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbssGatewayARN :: Lens.Lens' UpdateSMBSecurityStrategy Lude.Text
usmbssGatewayARN = Lens.lens (gatewayARN :: UpdateSMBSecurityStrategy -> Lude.Text) (\s a -> s {gatewayARN = a} :: UpdateSMBSecurityStrategy)
{-# DEPRECATED usmbssGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | Specifies the type of security strategy.
--
-- ClientSpecified: if you use this option, requests are established based on what is negotiated by the client. This option is recommended when you want to maximize compatibility across different clients in your environment.
-- MandatorySigning: if you use this option, file gateway only allows connections from SMBv2 or SMBv3 clients that have signing enabled. This option works with SMB clients on Microsoft Windows Vista, Windows Server 2008 or newer.
-- MandatoryEncryption: if you use this option, file gateway only allows connections from SMBv3 clients that have encryption enabled. This option is highly recommended for environments that handle sensitive data. This option works with SMB clients on Microsoft Windows 8, Windows Server 2012 or newer.
--
-- /Note:/ Consider using 'sMBSecurityStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbssSMBSecurityStrategy :: Lens.Lens' UpdateSMBSecurityStrategy SMBSecurityStrategy
usmbssSMBSecurityStrategy = Lens.lens (sMBSecurityStrategy :: UpdateSMBSecurityStrategy -> SMBSecurityStrategy) (\s a -> s {sMBSecurityStrategy = a} :: UpdateSMBSecurityStrategy)
{-# DEPRECATED usmbssSMBSecurityStrategy "Use generic-lens or generic-optics with 'sMBSecurityStrategy' instead." #-}

instance Lude.AWSRequest UpdateSMBSecurityStrategy where
  type
    Rs UpdateSMBSecurityStrategy =
      UpdateSMBSecurityStrategyResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateSMBSecurityStrategyResponse'
            Lude.<$> (x Lude..?> "GatewayARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateSMBSecurityStrategy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.UpdateSMBSecurityStrategy" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateSMBSecurityStrategy where
  toJSON UpdateSMBSecurityStrategy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("GatewayARN" Lude..= gatewayARN),
            Lude.Just ("SMBSecurityStrategy" Lude..= sMBSecurityStrategy)
          ]
      )

instance Lude.ToPath UpdateSMBSecurityStrategy where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateSMBSecurityStrategy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateSMBSecurityStrategyResponse' smart constructor.
data UpdateSMBSecurityStrategyResponse = UpdateSMBSecurityStrategyResponse'
  { gatewayARN ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'UpdateSMBSecurityStrategyResponse' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkUpdateSMBSecurityStrategyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateSMBSecurityStrategyResponse
mkUpdateSMBSecurityStrategyResponse pResponseStatus_ =
  UpdateSMBSecurityStrategyResponse'
    { gatewayARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbssrsGatewayARN :: Lens.Lens' UpdateSMBSecurityStrategyResponse (Lude.Maybe Lude.Text)
usmbssrsGatewayARN = Lens.lens (gatewayARN :: UpdateSMBSecurityStrategyResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: UpdateSMBSecurityStrategyResponse)
{-# DEPRECATED usmbssrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbssrsResponseStatus :: Lens.Lens' UpdateSMBSecurityStrategyResponse Lude.Int
usmbssrsResponseStatus = Lens.lens (responseStatus :: UpdateSMBSecurityStrategyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateSMBSecurityStrategyResponse)
{-# DEPRECATED usmbssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
