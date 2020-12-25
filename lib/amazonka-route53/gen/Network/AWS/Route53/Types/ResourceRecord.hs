{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.ResourceRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.ResourceRecord
  ( ResourceRecord (..),

    -- * Smart constructor
    mkResourceRecord,

    -- * Lenses
    rrValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53.Internal as Types
import qualified Network.AWS.Route53.Types.Value as Types

-- | Information specific to the resource record.
--
-- /See:/ 'mkResourceRecord' smart constructor.
newtype ResourceRecord = ResourceRecord'
  { -- | The current or new DNS record value, not to exceed 4,000 characters. In the case of a @DELETE@ action, if the current value does not match the actual value, an error is returned. For descriptions about how to format @Value@ for different record types, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/ResourceRecordTypes.html Supported DNS Resource Record Types> in the /Amazon Route 53 Developer Guide/ .
    --
    -- You can specify more than one value for all record types except @CNAME@ and @SOA@ .
    value :: Types.Value
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceRecord' value with any optional fields omitted.
mkResourceRecord ::
  -- | 'value'
  Types.Value ->
  ResourceRecord
mkResourceRecord value = ResourceRecord' {value}

-- | The current or new DNS record value, not to exceed 4,000 characters. In the case of a @DELETE@ action, if the current value does not match the actual value, an error is returned. For descriptions about how to format @Value@ for different record types, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/ResourceRecordTypes.html Supported DNS Resource Record Types> in the /Amazon Route 53 Developer Guide/ .
--
-- You can specify more than one value for all record types except @CNAME@ and @SOA@ .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrValue :: Lens.Lens' ResourceRecord Types.Value
rrValue = Lens.field @"value"
{-# DEPRECATED rrValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.ToXML ResourceRecord where
  toXML ResourceRecord {..} = Core.toXMLNode "Value" value

instance Core.FromXML ResourceRecord where
  parseXML x = ResourceRecord' Core.<$> (x Core..@ "Value")
