{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.SdkConfigurationProperty
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.SdkConfigurationProperty
  ( SdkConfigurationProperty (..)
  -- * Smart constructor
  , mkSdkConfigurationProperty
  -- * Lenses
  , scpDefaultValue
  , scpDescription
  , scpFriendlyName
  , scpName
  , scpRequired
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A configuration property of an SDK type.
--
-- /See:/ 'mkSdkConfigurationProperty' smart constructor.
data SdkConfigurationProperty = SdkConfigurationProperty'
  { defaultValue :: Core.Maybe Core.Text
    -- ^ The default value of an 'SdkType' configuration property.
  , description :: Core.Maybe Core.Text
    -- ^ The description of an 'SdkType' configuration property.
  , friendlyName :: Core.Maybe Core.Text
    -- ^ The user-friendly name of an 'SdkType' configuration property.
  , name :: Core.Maybe Core.Text
    -- ^ The name of a an 'SdkType' configuration property.
  , required :: Core.Maybe Core.Bool
    -- ^ A boolean flag of an 'SdkType' configuration property to indicate if the associated SDK configuration property is required (@true@ ) or not (@false@ ).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SdkConfigurationProperty' value with any optional fields omitted.
mkSdkConfigurationProperty
    :: SdkConfigurationProperty
mkSdkConfigurationProperty
  = SdkConfigurationProperty'{defaultValue = Core.Nothing,
                              description = Core.Nothing, friendlyName = Core.Nothing,
                              name = Core.Nothing, required = Core.Nothing}

-- | The default value of an 'SdkType' configuration property.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scpDefaultValue :: Lens.Lens' SdkConfigurationProperty (Core.Maybe Core.Text)
scpDefaultValue = Lens.field @"defaultValue"
{-# INLINEABLE scpDefaultValue #-}
{-# DEPRECATED defaultValue "Use generic-lens or generic-optics with 'defaultValue' instead"  #-}

-- | The description of an 'SdkType' configuration property.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scpDescription :: Lens.Lens' SdkConfigurationProperty (Core.Maybe Core.Text)
scpDescription = Lens.field @"description"
{-# INLINEABLE scpDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The user-friendly name of an 'SdkType' configuration property.
--
-- /Note:/ Consider using 'friendlyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scpFriendlyName :: Lens.Lens' SdkConfigurationProperty (Core.Maybe Core.Text)
scpFriendlyName = Lens.field @"friendlyName"
{-# INLINEABLE scpFriendlyName #-}
{-# DEPRECATED friendlyName "Use generic-lens or generic-optics with 'friendlyName' instead"  #-}

-- | The name of a an 'SdkType' configuration property.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scpName :: Lens.Lens' SdkConfigurationProperty (Core.Maybe Core.Text)
scpName = Lens.field @"name"
{-# INLINEABLE scpName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A boolean flag of an 'SdkType' configuration property to indicate if the associated SDK configuration property is required (@true@ ) or not (@false@ ).
--
-- /Note:/ Consider using 'required' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scpRequired :: Lens.Lens' SdkConfigurationProperty (Core.Maybe Core.Bool)
scpRequired = Lens.field @"required"
{-# INLINEABLE scpRequired #-}
{-# DEPRECATED required "Use generic-lens or generic-optics with 'required' instead"  #-}

instance Core.FromJSON SdkConfigurationProperty where
        parseJSON
          = Core.withObject "SdkConfigurationProperty" Core.$
              \ x ->
                SdkConfigurationProperty' Core.<$>
                  (x Core..:? "defaultValue") Core.<*> x Core..:? "description"
                    Core.<*> x Core..:? "friendlyName"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "required"
