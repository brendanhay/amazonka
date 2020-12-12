{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.Types.Endpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SNS.Types.Endpoint
  ( Endpoint (..),

    -- * Smart constructor
    mkEndpoint,

    -- * Lenses
    eAttributes,
    eEndpointARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Endpoint for mobile app and device.
--
-- /See:/ 'mkEndpoint' smart constructor.
data Endpoint = Endpoint'
  { attributes ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    endpointARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Endpoint' with the minimum fields required to make a request.
--
-- * 'attributes' - Attributes for endpoint.
-- * 'endpointARN' - EndpointArn for mobile app and device.
mkEndpoint ::
  Endpoint
mkEndpoint =
  Endpoint' {attributes = Lude.Nothing, endpointARN = Lude.Nothing}

-- | Attributes for endpoint.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAttributes :: Lens.Lens' Endpoint (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
eAttributes = Lens.lens (attributes :: Endpoint -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {attributes = a} :: Endpoint)
{-# DEPRECATED eAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | EndpointArn for mobile app and device.
--
-- /Note:/ Consider using 'endpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEndpointARN :: Lens.Lens' Endpoint (Lude.Maybe Lude.Text)
eEndpointARN = Lens.lens (endpointARN :: Endpoint -> Lude.Maybe Lude.Text) (\s a -> s {endpointARN = a} :: Endpoint)
{-# DEPRECATED eEndpointARN "Use generic-lens or generic-optics with 'endpointARN' instead." #-}

instance Lude.FromXML Endpoint where
  parseXML x =
    Endpoint'
      Lude.<$> ( x Lude..@? "Attributes" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLMap "entry" "key" "value")
               )
      Lude.<*> (x Lude..@? "EndpointArn")
