{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.AccessEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.AccessEndpoint
  ( AccessEndpoint (..),

    -- * Smart constructor
    mkAccessEndpoint,

    -- * Lenses
    aeVPCeId,
    aeEndpointType,
  )
where

import Network.AWS.AppStream.Types.AccessEndpointType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an interface VPC endpoint (interface endpoint) that lets you create a private connection between the virtual private cloud (VPC) that you specify and AppStream 2.0. When you specify an interface endpoint for a stack, users of the stack can connect to AppStream 2.0 only through that endpoint. When you specify an interface endpoint for an image builder, administrators can connect to the image builder only through that endpoint.
--
-- /See:/ 'mkAccessEndpoint' smart constructor.
data AccessEndpoint = AccessEndpoint'
  { vpceId ::
      Lude.Maybe Lude.Text,
    endpointType :: AccessEndpointType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AccessEndpoint' with the minimum fields required to make a request.
--
-- * 'endpointType' - The type of interface endpoint.
-- * 'vpceId' - The identifier (ID) of the VPC in which the interface endpoint is used.
mkAccessEndpoint ::
  -- | 'endpointType'
  AccessEndpointType ->
  AccessEndpoint
mkAccessEndpoint pEndpointType_ =
  AccessEndpoint'
    { vpceId = Lude.Nothing,
      endpointType = pEndpointType_
    }

-- | The identifier (ID) of the VPC in which the interface endpoint is used.
--
-- /Note:/ Consider using 'vpceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeVPCeId :: Lens.Lens' AccessEndpoint (Lude.Maybe Lude.Text)
aeVPCeId = Lens.lens (vpceId :: AccessEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {vpceId = a} :: AccessEndpoint)
{-# DEPRECATED aeVPCeId "Use generic-lens or generic-optics with 'vpceId' instead." #-}

-- | The type of interface endpoint.
--
-- /Note:/ Consider using 'endpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeEndpointType :: Lens.Lens' AccessEndpoint AccessEndpointType
aeEndpointType = Lens.lens (endpointType :: AccessEndpoint -> AccessEndpointType) (\s a -> s {endpointType = a} :: AccessEndpoint)
{-# DEPRECATED aeEndpointType "Use generic-lens or generic-optics with 'endpointType' instead." #-}

instance Lude.FromJSON AccessEndpoint where
  parseJSON =
    Lude.withObject
      "AccessEndpoint"
      ( \x ->
          AccessEndpoint'
            Lude.<$> (x Lude..:? "VpceId") Lude.<*> (x Lude..: "EndpointType")
      )

instance Lude.ToJSON AccessEndpoint where
  toJSON AccessEndpoint' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("VpceId" Lude..=) Lude.<$> vpceId,
            Lude.Just ("EndpointType" Lude..= endpointType)
          ]
      )
