{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.Nameserver
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.Nameserver
  ( Nameserver (..),

    -- * Smart constructor
    mkNameserver,

    -- * Lenses
    nName,
    nGlueIPs,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Nameserver includes the following elements.
--
-- /See:/ 'mkNameserver' smart constructor.
data Nameserver = Nameserver'
  { -- | The fully qualified host name of the name server.
    --
    -- Constraint: Maximum 255 characters
    name :: Lude.Text,
    -- | Glue IP address of a name server entry. Glue IP addresses are required only when the name of the name server is a subdomain of the domain. For example, if your domain is example.com and the name server for the domain is ns.example.com, you need to specify the IP address for ns.example.com.
    --
    -- Constraints: The list can contain only one IPv4 and one IPv6 address.
    glueIPs :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Nameserver' with the minimum fields required to make a request.
--
-- * 'name' - The fully qualified host name of the name server.
--
-- Constraint: Maximum 255 characters
-- * 'glueIPs' - Glue IP address of a name server entry. Glue IP addresses are required only when the name of the name server is a subdomain of the domain. For example, if your domain is example.com and the name server for the domain is ns.example.com, you need to specify the IP address for ns.example.com.
--
-- Constraints: The list can contain only one IPv4 and one IPv6 address.
mkNameserver ::
  -- | 'name'
  Lude.Text ->
  Nameserver
mkNameserver pName_ =
  Nameserver' {name = pName_, glueIPs = Lude.Nothing}

-- | The fully qualified host name of the name server.
--
-- Constraint: Maximum 255 characters
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nName :: Lens.Lens' Nameserver Lude.Text
nName = Lens.lens (name :: Nameserver -> Lude.Text) (\s a -> s {name = a} :: Nameserver)
{-# DEPRECATED nName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Glue IP address of a name server entry. Glue IP addresses are required only when the name of the name server is a subdomain of the domain. For example, if your domain is example.com and the name server for the domain is ns.example.com, you need to specify the IP address for ns.example.com.
--
-- Constraints: The list can contain only one IPv4 and one IPv6 address.
--
-- /Note:/ Consider using 'glueIPs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nGlueIPs :: Lens.Lens' Nameserver (Lude.Maybe [Lude.Text])
nGlueIPs = Lens.lens (glueIPs :: Nameserver -> Lude.Maybe [Lude.Text]) (\s a -> s {glueIPs = a} :: Nameserver)
{-# DEPRECATED nGlueIPs "Use generic-lens or generic-optics with 'glueIPs' instead." #-}

instance Lude.FromJSON Nameserver where
  parseJSON =
    Lude.withObject
      "Nameserver"
      ( \x ->
          Nameserver'
            Lude.<$> (x Lude..: "Name")
            Lude.<*> (x Lude..:? "GlueIps" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON Nameserver where
  toJSON Nameserver' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            ("GlueIps" Lude..=) Lude.<$> glueIPs
          ]
      )
