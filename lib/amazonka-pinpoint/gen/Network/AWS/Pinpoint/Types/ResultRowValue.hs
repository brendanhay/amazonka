{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ResultRowValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.ResultRowValue
  ( ResultRowValue (..)
  -- * Smart constructor
  , mkResultRowValue
  -- * Lenses
  , rrvType
  , rrvValue
  , rrvKey
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides a single value and metadata about that value as part of an array of query results for a standard metric that applies to an application, campaign, or journey.
--
-- /See:/ 'mkResultRowValue' smart constructor.
data ResultRowValue = ResultRowValue'
  { type' :: Core.Text
    -- ^ The data type of the value specified by the Value property.
  , value :: Core.Text
    -- ^ In a Values object, the value for the metric that the query retrieved data for. In a GroupedBys object, the value for the field that was used to group data in a result set that contains multiple results (Values objects).
  , key :: Core.Text
    -- ^ The friendly name of the metric whose value is specified by the Value property.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResultRowValue' value with any optional fields omitted.
mkResultRowValue
    :: Core.Text -- ^ 'type\''
    -> Core.Text -- ^ 'value'
    -> Core.Text -- ^ 'key'
    -> ResultRowValue
mkResultRowValue type' value key
  = ResultRowValue'{type', value, key}

-- | The data type of the value specified by the Value property.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrvType :: Lens.Lens' ResultRowValue Core.Text
rrvType = Lens.field @"type'"
{-# INLINEABLE rrvType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | In a Values object, the value for the metric that the query retrieved data for. In a GroupedBys object, the value for the field that was used to group data in a result set that contains multiple results (Values objects).
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrvValue :: Lens.Lens' ResultRowValue Core.Text
rrvValue = Lens.field @"value"
{-# INLINEABLE rrvValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

-- | The friendly name of the metric whose value is specified by the Value property.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrvKey :: Lens.Lens' ResultRowValue Core.Text
rrvKey = Lens.field @"key"
{-# INLINEABLE rrvKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

instance Core.FromJSON ResultRowValue where
        parseJSON
          = Core.withObject "ResultRowValue" Core.$
              \ x ->
                ResultRowValue' Core.<$>
                  (x Core..: "Type") Core.<*> x Core..: "Value" Core.<*>
                    x Core..: "Key"
