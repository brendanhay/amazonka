{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.UsagePlanKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.UsagePlanKey
  ( UsagePlanKey (..)
  -- * Smart constructor
  , mkUsagePlanKey
  -- * Lenses
  , upkId
  , upkName
  , upkType
  , upkValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a usage plan key to identify a plan customer.
--
-- To associate an API stage with a selected API key in a usage plan, you must create a UsagePlanKey resource to represent the selected 'ApiKey' .
-- " <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-api-usage-plans.html Create and Use Usage Plans> 
--
-- /See:/ 'mkUsagePlanKey' smart constructor.
data UsagePlanKey = UsagePlanKey'
  { id :: Core.Maybe Core.Text
    -- ^ The Id of a usage plan key.
  , name :: Core.Maybe Core.Text
    -- ^ The name of a usage plan key.
  , type' :: Core.Maybe Core.Text
    -- ^ The type of a usage plan key. Currently, the valid key type is @API_KEY@ .
  , value :: Core.Maybe Core.Text
    -- ^ The value of a usage plan key.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UsagePlanKey' value with any optional fields omitted.
mkUsagePlanKey
    :: UsagePlanKey
mkUsagePlanKey
  = UsagePlanKey'{id = Core.Nothing, name = Core.Nothing,
                  type' = Core.Nothing, value = Core.Nothing}

-- | The Id of a usage plan key.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upkId :: Lens.Lens' UsagePlanKey (Core.Maybe Core.Text)
upkId = Lens.field @"id"
{-# INLINEABLE upkId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The name of a usage plan key.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upkName :: Lens.Lens' UsagePlanKey (Core.Maybe Core.Text)
upkName = Lens.field @"name"
{-# INLINEABLE upkName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The type of a usage plan key. Currently, the valid key type is @API_KEY@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upkType :: Lens.Lens' UsagePlanKey (Core.Maybe Core.Text)
upkType = Lens.field @"type'"
{-# INLINEABLE upkType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The value of a usage plan key.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upkValue :: Lens.Lens' UsagePlanKey (Core.Maybe Core.Text)
upkValue = Lens.field @"value"
{-# INLINEABLE upkValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON UsagePlanKey where
        parseJSON
          = Core.withObject "UsagePlanKey" Core.$
              \ x ->
                UsagePlanKey' Core.<$>
                  (x Core..:? "id") Core.<*> x Core..:? "name" Core.<*>
                    x Core..:? "type"
                    Core.<*> x Core..:? "value"
