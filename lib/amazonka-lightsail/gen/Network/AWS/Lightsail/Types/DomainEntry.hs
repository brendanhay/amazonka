{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.DomainEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.DomainEntry
  ( DomainEntry (..)
  -- * Smart constructor
  , mkDomainEntry
  -- * Lenses
  , deId
  , deIsAlias
  , deName
  , deOptions
  , deTarget
  , deType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.DomainEntryOptionsKeys as Types
import qualified Network.AWS.Lightsail.Types.DomainEntryType as Types
import qualified Network.AWS.Lightsail.Types.Id as Types
import qualified Network.AWS.Lightsail.Types.Name as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a domain recordset entry.
--
-- /See:/ 'mkDomainEntry' smart constructor.
data DomainEntry = DomainEntry'
  { id :: Core.Maybe Types.Id
    -- ^ The ID of the domain recordset entry.
  , isAlias :: Core.Maybe Core.Bool
    -- ^ When @true@ , specifies whether the domain entry is an alias used by the Lightsail load balancer. You can include an alias (A type) record in your request, which points to a load balancer DNS name and routes traffic to your load balancer.
  , name :: Core.Maybe Types.Name
    -- ^ The name of the domain.
  , options :: Core.Maybe (Core.HashMap Types.DomainEntryOptionsKeys Core.Text)
    -- ^ (Deprecated) The options for the domain entry.
  , target :: Core.Maybe Core.Text
    -- ^ The target AWS name server (e.g., @ns-111.awsdns-22.com.@ ).
--
-- For Lightsail load balancers, the value looks like @ab1234c56789c6b86aba6fb203d443bc-123456789.us-east-2.elb.amazonaws.com@ . Be sure to also set @isAlias@ to @true@ when setting up an A record for a load balancer.
  , type' :: Core.Maybe Types.DomainEntryType
    -- ^ The type of domain entry, such as address (A), canonical name (CNAME), mail exchanger (MX), name server (NS), start of authority (SOA), service locator (SRV), or text (TXT).
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DomainEntry' value with any optional fields omitted.
mkDomainEntry
    :: DomainEntry
mkDomainEntry
  = DomainEntry'{id = Core.Nothing, isAlias = Core.Nothing,
                 name = Core.Nothing, options = Core.Nothing, target = Core.Nothing,
                 type' = Core.Nothing}

-- | The ID of the domain recordset entry.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deId :: Lens.Lens' DomainEntry (Core.Maybe Types.Id)
deId = Lens.field @"id"
{-# INLINEABLE deId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | When @true@ , specifies whether the domain entry is an alias used by the Lightsail load balancer. You can include an alias (A type) record in your request, which points to a load balancer DNS name and routes traffic to your load balancer.
--
-- /Note:/ Consider using 'isAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deIsAlias :: Lens.Lens' DomainEntry (Core.Maybe Core.Bool)
deIsAlias = Lens.field @"isAlias"
{-# INLINEABLE deIsAlias #-}
{-# DEPRECATED isAlias "Use generic-lens or generic-optics with 'isAlias' instead"  #-}

-- | The name of the domain.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deName :: Lens.Lens' DomainEntry (Core.Maybe Types.Name)
deName = Lens.field @"name"
{-# INLINEABLE deName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | (Deprecated) The options for the domain entry.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deOptions :: Lens.Lens' DomainEntry (Core.Maybe (Core.HashMap Types.DomainEntryOptionsKeys Core.Text))
deOptions = Lens.field @"options"
{-# INLINEABLE deOptions #-}
{-# DEPRECATED options "Use generic-lens or generic-optics with 'options' instead"  #-}

-- | The target AWS name server (e.g., @ns-111.awsdns-22.com.@ ).
--
-- For Lightsail load balancers, the value looks like @ab1234c56789c6b86aba6fb203d443bc-123456789.us-east-2.elb.amazonaws.com@ . Be sure to also set @isAlias@ to @true@ when setting up an A record for a load balancer.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deTarget :: Lens.Lens' DomainEntry (Core.Maybe Core.Text)
deTarget = Lens.field @"target"
{-# INLINEABLE deTarget #-}
{-# DEPRECATED target "Use generic-lens or generic-optics with 'target' instead"  #-}

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
deType :: Lens.Lens' DomainEntry (Core.Maybe Types.DomainEntryType)
deType = Lens.field @"type'"
{-# INLINEABLE deType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON DomainEntry where
        toJSON DomainEntry{..}
          = Core.object
              (Core.catMaybes
                 [("id" Core..=) Core.<$> id, ("isAlias" Core..=) Core.<$> isAlias,
                  ("name" Core..=) Core.<$> name,
                  ("options" Core..=) Core.<$> options,
                  ("target" Core..=) Core.<$> target,
                  ("type" Core..=) Core.<$> type'])

instance Core.FromJSON DomainEntry where
        parseJSON
          = Core.withObject "DomainEntry" Core.$
              \ x ->
                DomainEntry' Core.<$>
                  (x Core..:? "id") Core.<*> x Core..:? "isAlias" Core.<*>
                    x Core..:? "name"
                    Core.<*> x Core..:? "options"
                    Core.<*> x Core..:? "target"
                    Core.<*> x Core..:? "type"
