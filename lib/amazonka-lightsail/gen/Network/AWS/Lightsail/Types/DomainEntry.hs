{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.DomainEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.DomainEntry
  ( DomainEntry (..),

    -- * Smart constructor
    mkDomainEntry,

    -- * Lenses
    deIsAlias,
    deName,
    deId,
    deOptions,
    deType,
    deTarget,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a domain recordset entry.
--
-- /See:/ 'mkDomainEntry' smart constructor.
data DomainEntry = DomainEntry'
  { -- | When @true@ , specifies whether the domain entry is an alias used by the Lightsail load balancer. You can include an alias (A type) record in your request, which points to a load balancer DNS name and routes traffic to your load balancer.
    isAlias :: Lude.Maybe Lude.Bool,
    -- | The name of the domain.
    name :: Lude.Maybe Lude.Text,
    -- | The ID of the domain recordset entry.
    id :: Lude.Maybe Lude.Text,
    -- | (Deprecated) The options for the domain entry.
    options :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The type of domain entry, such as address (A), canonical name (CNAME), mail exchanger (MX), name server (NS), start of authority (SOA), service locator (SRV), or text (TXT).
    --
    -- The following domain entry types can be used:
    --
    --     * @A@
    --
    --
    --     * @CNAME@
    --
    --
    --     * @MX@
    --
    --
    --     * @NS@
    --
    --
    --     * @SOA@
    --
    --
    --     * @SRV@
    --
    --
    --     * @TXT@
    type' :: Lude.Maybe Lude.Text,
    -- | The target AWS name server (e.g., @ns-111.awsdns-22.com.@ ).
    --
    -- For Lightsail load balancers, the value looks like @ab1234c56789c6b86aba6fb203d443bc-123456789.us-east-2.elb.amazonaws.com@ . Be sure to also set @isAlias@ to @true@ when setting up an A record for a load balancer.
    target :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DomainEntry' with the minimum fields required to make a request.
--
-- * 'isAlias' - When @true@ , specifies whether the domain entry is an alias used by the Lightsail load balancer. You can include an alias (A type) record in your request, which points to a load balancer DNS name and routes traffic to your load balancer.
-- * 'name' - The name of the domain.
-- * 'id' - The ID of the domain recordset entry.
-- * 'options' - (Deprecated) The options for the domain entry.
-- * 'type'' - The type of domain entry, such as address (A), canonical name (CNAME), mail exchanger (MX), name server (NS), start of authority (SOA), service locator (SRV), or text (TXT).
--
-- The following domain entry types can be used:
--
--     * @A@
--
--
--     * @CNAME@
--
--
--     * @MX@
--
--
--     * @NS@
--
--
--     * @SOA@
--
--
--     * @SRV@
--
--
--     * @TXT@
--
--
-- * 'target' - The target AWS name server (e.g., @ns-111.awsdns-22.com.@ ).
--
-- For Lightsail load balancers, the value looks like @ab1234c56789c6b86aba6fb203d443bc-123456789.us-east-2.elb.amazonaws.com@ . Be sure to also set @isAlias@ to @true@ when setting up an A record for a load balancer.
mkDomainEntry ::
  DomainEntry
mkDomainEntry =
  DomainEntry'
    { isAlias = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      options = Lude.Nothing,
      type' = Lude.Nothing,
      target = Lude.Nothing
    }

-- | When @true@ , specifies whether the domain entry is an alias used by the Lightsail load balancer. You can include an alias (A type) record in your request, which points to a load balancer DNS name and routes traffic to your load balancer.
--
-- /Note:/ Consider using 'isAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deIsAlias :: Lens.Lens' DomainEntry (Lude.Maybe Lude.Bool)
deIsAlias = Lens.lens (isAlias :: DomainEntry -> Lude.Maybe Lude.Bool) (\s a -> s {isAlias = a} :: DomainEntry)
{-# DEPRECATED deIsAlias "Use generic-lens or generic-optics with 'isAlias' instead." #-}

-- | The name of the domain.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deName :: Lens.Lens' DomainEntry (Lude.Maybe Lude.Text)
deName = Lens.lens (name :: DomainEntry -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DomainEntry)
{-# DEPRECATED deName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the domain recordset entry.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deId :: Lens.Lens' DomainEntry (Lude.Maybe Lude.Text)
deId = Lens.lens (id :: DomainEntry -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: DomainEntry)
{-# DEPRECATED deId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | (Deprecated) The options for the domain entry.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deOptions :: Lens.Lens' DomainEntry (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
deOptions = Lens.lens (options :: DomainEntry -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {options = a} :: DomainEntry)
{-# DEPRECATED deOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | The type of domain entry, such as address (A), canonical name (CNAME), mail exchanger (MX), name server (NS), start of authority (SOA), service locator (SRV), or text (TXT).
--
-- The following domain entry types can be used:
--
--     * @A@
--
--
--     * @CNAME@
--
--
--     * @MX@
--
--
--     * @NS@
--
--
--     * @SOA@
--
--
--     * @SRV@
--
--
--     * @TXT@
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deType :: Lens.Lens' DomainEntry (Lude.Maybe Lude.Text)
deType = Lens.lens (type' :: DomainEntry -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: DomainEntry)
{-# DEPRECATED deType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The target AWS name server (e.g., @ns-111.awsdns-22.com.@ ).
--
-- For Lightsail load balancers, the value looks like @ab1234c56789c6b86aba6fb203d443bc-123456789.us-east-2.elb.amazonaws.com@ . Be sure to also set @isAlias@ to @true@ when setting up an A record for a load balancer.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deTarget :: Lens.Lens' DomainEntry (Lude.Maybe Lude.Text)
deTarget = Lens.lens (target :: DomainEntry -> Lude.Maybe Lude.Text) (\s a -> s {target = a} :: DomainEntry)
{-# DEPRECATED deTarget "Use generic-lens or generic-optics with 'target' instead." #-}

instance Lude.FromJSON DomainEntry where
  parseJSON =
    Lude.withObject
      "DomainEntry"
      ( \x ->
          DomainEntry'
            Lude.<$> (x Lude..:? "isAlias")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "options" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "type")
            Lude.<*> (x Lude..:? "target")
      )

instance Lude.ToJSON DomainEntry where
  toJSON DomainEntry' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("isAlias" Lude..=) Lude.<$> isAlias,
            ("name" Lude..=) Lude.<$> name,
            ("id" Lude..=) Lude.<$> id,
            ("options" Lude..=) Lude.<$> options,
            ("type" Lude..=) Lude.<$> type',
            ("target" Lude..=) Lude.<$> target
          ]
      )
