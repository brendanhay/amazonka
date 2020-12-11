{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.PutConfigurationSetDeliveryOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates the delivery options for a configuration set.
module Network.AWS.SES.PutConfigurationSetDeliveryOptions
  ( -- * Creating a request
    PutConfigurationSetDeliveryOptions (..),
    mkPutConfigurationSetDeliveryOptions,

    -- ** Request lenses
    pcsdoDeliveryOptions,
    pcsdoConfigurationSetName,

    -- * Destructuring the response
    PutConfigurationSetDeliveryOptionsResponse (..),
    mkPutConfigurationSetDeliveryOptionsResponse,

    -- ** Response lenses
    pcsdorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | A request to modify the delivery options for a configuration set.
--
-- /See:/ 'mkPutConfigurationSetDeliveryOptions' smart constructor.
data PutConfigurationSetDeliveryOptions = PutConfigurationSetDeliveryOptions'
  { deliveryOptions ::
      Lude.Maybe
        DeliveryOptions,
    configurationSetName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutConfigurationSetDeliveryOptions' with the minimum fields required to make a request.
--
-- * 'configurationSetName' - The name of the configuration set that you want to specify the delivery options for.
-- * 'deliveryOptions' - Specifies whether messages that use the configuration set are required to use Transport Layer Security (TLS).
mkPutConfigurationSetDeliveryOptions ::
  -- | 'configurationSetName'
  Lude.Text ->
  PutConfigurationSetDeliveryOptions
mkPutConfigurationSetDeliveryOptions pConfigurationSetName_ =
  PutConfigurationSetDeliveryOptions'
    { deliveryOptions =
        Lude.Nothing,
      configurationSetName = pConfigurationSetName_
    }

-- | Specifies whether messages that use the configuration set are required to use Transport Layer Security (TLS).
--
-- /Note:/ Consider using 'deliveryOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcsdoDeliveryOptions :: Lens.Lens' PutConfigurationSetDeliveryOptions (Lude.Maybe DeliveryOptions)
pcsdoDeliveryOptions = Lens.lens (deliveryOptions :: PutConfigurationSetDeliveryOptions -> Lude.Maybe DeliveryOptions) (\s a -> s {deliveryOptions = a} :: PutConfigurationSetDeliveryOptions)
{-# DEPRECATED pcsdoDeliveryOptions "Use generic-lens or generic-optics with 'deliveryOptions' instead." #-}

-- | The name of the configuration set that you want to specify the delivery options for.
--
-- /Note:/ Consider using 'configurationSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcsdoConfigurationSetName :: Lens.Lens' PutConfigurationSetDeliveryOptions Lude.Text
pcsdoConfigurationSetName = Lens.lens (configurationSetName :: PutConfigurationSetDeliveryOptions -> Lude.Text) (\s a -> s {configurationSetName = a} :: PutConfigurationSetDeliveryOptions)
{-# DEPRECATED pcsdoConfigurationSetName "Use generic-lens or generic-optics with 'configurationSetName' instead." #-}

instance Lude.AWSRequest PutConfigurationSetDeliveryOptions where
  type
    Rs PutConfigurationSetDeliveryOptions =
      PutConfigurationSetDeliveryOptionsResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "PutConfigurationSetDeliveryOptionsResult"
      ( \s h x ->
          PutConfigurationSetDeliveryOptionsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutConfigurationSetDeliveryOptions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath PutConfigurationSetDeliveryOptions where
  toPath = Lude.const "/"

instance Lude.ToQuery PutConfigurationSetDeliveryOptions where
  toQuery PutConfigurationSetDeliveryOptions' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("PutConfigurationSetDeliveryOptions" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "DeliveryOptions" Lude.=: deliveryOptions,
        "ConfigurationSetName" Lude.=: configurationSetName
      ]

-- | An HTTP 200 response if the request succeeds, or an error message if the request fails.
--
-- /See:/ 'mkPutConfigurationSetDeliveryOptionsResponse' smart constructor.
newtype PutConfigurationSetDeliveryOptionsResponse = PutConfigurationSetDeliveryOptionsResponse'
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

-- | Creates a value of 'PutConfigurationSetDeliveryOptionsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkPutConfigurationSetDeliveryOptionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutConfigurationSetDeliveryOptionsResponse
mkPutConfigurationSetDeliveryOptionsResponse pResponseStatus_ =
  PutConfigurationSetDeliveryOptionsResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcsdorsResponseStatus :: Lens.Lens' PutConfigurationSetDeliveryOptionsResponse Lude.Int
pcsdorsResponseStatus = Lens.lens (responseStatus :: PutConfigurationSetDeliveryOptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutConfigurationSetDeliveryOptionsResponse)
{-# DEPRECATED pcsdorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
