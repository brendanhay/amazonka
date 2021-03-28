{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.SdkType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.SdkType
  ( SdkType (..)
  -- * Smart constructor
  , mkSdkType
  -- * Lenses
  , stConfigurationProperties
  , stDescription
  , stFriendlyName
  , stId
  ) where

import qualified Network.AWS.ApiGateway.Types.SdkConfigurationProperty as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A type of SDK that API Gateway can generate.
--
-- /See:/ 'mkSdkType' smart constructor.
data SdkType = SdkType'
  { configurationProperties :: Core.Maybe [Types.SdkConfigurationProperty]
    -- ^ A list of configuration properties of an 'SdkType' .
  , description :: Core.Maybe Core.Text
    -- ^ The description of an 'SdkType' .
  , friendlyName :: Core.Maybe Core.Text
    -- ^ The user-friendly name of an 'SdkType' instance.
  , id :: Core.Maybe Core.Text
    -- ^ The identifier of an 'SdkType' instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SdkType' value with any optional fields omitted.
mkSdkType
    :: SdkType
mkSdkType
  = SdkType'{configurationProperties = Core.Nothing,
             description = Core.Nothing, friendlyName = Core.Nothing,
             id = Core.Nothing}

-- | A list of configuration properties of an 'SdkType' .
--
-- /Note:/ Consider using 'configurationProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stConfigurationProperties :: Lens.Lens' SdkType (Core.Maybe [Types.SdkConfigurationProperty])
stConfigurationProperties = Lens.field @"configurationProperties"
{-# INLINEABLE stConfigurationProperties #-}
{-# DEPRECATED configurationProperties "Use generic-lens or generic-optics with 'configurationProperties' instead"  #-}

-- | The description of an 'SdkType' .
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stDescription :: Lens.Lens' SdkType (Core.Maybe Core.Text)
stDescription = Lens.field @"description"
{-# INLINEABLE stDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The user-friendly name of an 'SdkType' instance.
--
-- /Note:/ Consider using 'friendlyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stFriendlyName :: Lens.Lens' SdkType (Core.Maybe Core.Text)
stFriendlyName = Lens.field @"friendlyName"
{-# INLINEABLE stFriendlyName #-}
{-# DEPRECATED friendlyName "Use generic-lens or generic-optics with 'friendlyName' instead"  #-}

-- | The identifier of an 'SdkType' instance.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stId :: Lens.Lens' SdkType (Core.Maybe Core.Text)
stId = Lens.field @"id"
{-# INLINEABLE stId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.FromJSON SdkType where
        parseJSON
          = Core.withObject "SdkType" Core.$
              \ x ->
                SdkType' Core.<$>
                  (x Core..:? "configurationProperties") Core.<*>
                    x Core..:? "description"
                    Core.<*> x Core..:? "friendlyName"
                    Core.<*> x Core..:? "id"
