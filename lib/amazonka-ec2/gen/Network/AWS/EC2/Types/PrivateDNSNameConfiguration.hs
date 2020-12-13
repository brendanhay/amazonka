{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PrivateDNSNameConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PrivateDNSNameConfiguration
  ( PrivateDNSNameConfiguration (..),

    -- * Smart constructor
    mkPrivateDNSNameConfiguration,

    -- * Lenses
    pdncState,
    pdncValue,
    pdncName,
    pdncType,
  )
where

import Network.AWS.EC2.Types.DNSNameState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the private DNS name for the service endpoint. For more information about these parameters, see <https://docs.aws.amazon.com/vpc/latest/userguide/ndpoint-services-dns-validation.html VPC Endpoint Service Private DNS Name Verification> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- /See:/ 'mkPrivateDNSNameConfiguration' smart constructor.
data PrivateDNSNameConfiguration = PrivateDNSNameConfiguration'
  { -- | The verification state of the VPC endpoint service.
    --
    -- >Consumers of the endpoint service can use the private name only when the state is @verified@ .
    state :: Lude.Maybe DNSNameState,
    -- | The value the service provider adds to the private DNS name domain record before verification.
    value :: Lude.Maybe Lude.Text,
    -- | The name of the record subdomain the service provider needs to create. The service provider adds the @value@ text to the @name@ .
    name :: Lude.Maybe Lude.Text,
    -- | The endpoint service verification type, for example TXT.
    type' :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PrivateDNSNameConfiguration' with the minimum fields required to make a request.
--
-- * 'state' - The verification state of the VPC endpoint service.
--
-- >Consumers of the endpoint service can use the private name only when the state is @verified@ .
-- * 'value' - The value the service provider adds to the private DNS name domain record before verification.
-- * 'name' - The name of the record subdomain the service provider needs to create. The service provider adds the @value@ text to the @name@ .
-- * 'type'' - The endpoint service verification type, for example TXT.
mkPrivateDNSNameConfiguration ::
  PrivateDNSNameConfiguration
mkPrivateDNSNameConfiguration =
  PrivateDNSNameConfiguration'
    { state = Lude.Nothing,
      value = Lude.Nothing,
      name = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | The verification state of the VPC endpoint service.
--
-- >Consumers of the endpoint service can use the private name only when the state is @verified@ .
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdncState :: Lens.Lens' PrivateDNSNameConfiguration (Lude.Maybe DNSNameState)
pdncState = Lens.lens (state :: PrivateDNSNameConfiguration -> Lude.Maybe DNSNameState) (\s a -> s {state = a} :: PrivateDNSNameConfiguration)
{-# DEPRECATED pdncState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The value the service provider adds to the private DNS name domain record before verification.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdncValue :: Lens.Lens' PrivateDNSNameConfiguration (Lude.Maybe Lude.Text)
pdncValue = Lens.lens (value :: PrivateDNSNameConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: PrivateDNSNameConfiguration)
{-# DEPRECATED pdncValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The name of the record subdomain the service provider needs to create. The service provider adds the @value@ text to the @name@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdncName :: Lens.Lens' PrivateDNSNameConfiguration (Lude.Maybe Lude.Text)
pdncName = Lens.lens (name :: PrivateDNSNameConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: PrivateDNSNameConfiguration)
{-# DEPRECATED pdncName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The endpoint service verification type, for example TXT.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdncType :: Lens.Lens' PrivateDNSNameConfiguration (Lude.Maybe Lude.Text)
pdncType = Lens.lens (type' :: PrivateDNSNameConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: PrivateDNSNameConfiguration)
{-# DEPRECATED pdncType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromXML PrivateDNSNameConfiguration where
  parseXML x =
    PrivateDNSNameConfiguration'
      Lude.<$> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "value")
      Lude.<*> (x Lude..@? "name")
      Lude.<*> (x Lude..@? "type")
