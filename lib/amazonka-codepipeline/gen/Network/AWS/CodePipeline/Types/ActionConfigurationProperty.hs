{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionConfigurationProperty
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.ActionConfigurationProperty
  ( ActionConfigurationProperty (..)
  -- * Smart constructor
  , mkActionConfigurationProperty
  -- * Lenses
  , acpName
  , acpRequired
  , acpKey
  , acpSecret
  , acpDescription
  , acpQueryable
  , acpType
  ) where

import qualified Network.AWS.CodePipeline.Types.ActionConfigurationPropertyType as Types
import qualified Network.AWS.CodePipeline.Types.Description as Types
import qualified Network.AWS.CodePipeline.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents information about an action configuration property.
--
-- /See:/ 'mkActionConfigurationProperty' smart constructor.
data ActionConfigurationProperty = ActionConfigurationProperty'
  { name :: Types.Name
    -- ^ The name of the action configuration property.
  , required :: Core.Bool
    -- ^ Whether the configuration property is a required value.
  , key :: Core.Bool
    -- ^ Whether the configuration property is a key.
  , secret :: Core.Bool
    -- ^ Whether the configuration property is secret. Secrets are hidden from all calls except for @GetJobDetails@ , @GetThirdPartyJobDetails@ , @PollForJobs@ , and @PollForThirdPartyJobs@ .
--
-- When updating a pipeline, passing * * * * * without changing any other values of the action preserves the previous value of the secret.
  , description :: Core.Maybe Types.Description
    -- ^ The description of the action configuration property that is displayed to users.
  , queryable :: Core.Maybe Core.Bool
    -- ^ Indicates that the property is used with @PollForJobs@ . When creating a custom action, an action can have up to one queryable property. If it has one, that property must be both required and not secret.
--
-- If you create a pipeline with a custom action type, and that custom action contains a queryable property, the value for that configuration property is subject to other restrictions. The value must be less than or equal to twenty (20) characters. The value can contain only alphanumeric characters, underscores, and hyphens.
  , type' :: Core.Maybe Types.ActionConfigurationPropertyType
    -- ^ The type of the configuration property.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ActionConfigurationProperty' value with any optional fields omitted.
mkActionConfigurationProperty
    :: Types.Name -- ^ 'name'
    -> Core.Bool -- ^ 'required'
    -> Core.Bool -- ^ 'key'
    -> Core.Bool -- ^ 'secret'
    -> ActionConfigurationProperty
mkActionConfigurationProperty name required key secret
  = ActionConfigurationProperty'{name, required, key, secret,
                                 description = Core.Nothing, queryable = Core.Nothing,
                                 type' = Core.Nothing}

-- | The name of the action configuration property.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acpName :: Lens.Lens' ActionConfigurationProperty Types.Name
acpName = Lens.field @"name"
{-# INLINEABLE acpName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Whether the configuration property is a required value.
--
-- /Note:/ Consider using 'required' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acpRequired :: Lens.Lens' ActionConfigurationProperty Core.Bool
acpRequired = Lens.field @"required"
{-# INLINEABLE acpRequired #-}
{-# DEPRECATED required "Use generic-lens or generic-optics with 'required' instead"  #-}

-- | Whether the configuration property is a key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acpKey :: Lens.Lens' ActionConfigurationProperty Core.Bool
acpKey = Lens.field @"key"
{-# INLINEABLE acpKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | Whether the configuration property is secret. Secrets are hidden from all calls except for @GetJobDetails@ , @GetThirdPartyJobDetails@ , @PollForJobs@ , and @PollForThirdPartyJobs@ .
--
-- When updating a pipeline, passing * * * * * without changing any other values of the action preserves the previous value of the secret.
--
-- /Note:/ Consider using 'secret' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acpSecret :: Lens.Lens' ActionConfigurationProperty Core.Bool
acpSecret = Lens.field @"secret"
{-# INLINEABLE acpSecret #-}
{-# DEPRECATED secret "Use generic-lens or generic-optics with 'secret' instead"  #-}

-- | The description of the action configuration property that is displayed to users.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acpDescription :: Lens.Lens' ActionConfigurationProperty (Core.Maybe Types.Description)
acpDescription = Lens.field @"description"
{-# INLINEABLE acpDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Indicates that the property is used with @PollForJobs@ . When creating a custom action, an action can have up to one queryable property. If it has one, that property must be both required and not secret.
--
-- If you create a pipeline with a custom action type, and that custom action contains a queryable property, the value for that configuration property is subject to other restrictions. The value must be less than or equal to twenty (20) characters. The value can contain only alphanumeric characters, underscores, and hyphens.
--
-- /Note:/ Consider using 'queryable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acpQueryable :: Lens.Lens' ActionConfigurationProperty (Core.Maybe Core.Bool)
acpQueryable = Lens.field @"queryable"
{-# INLINEABLE acpQueryable #-}
{-# DEPRECATED queryable "Use generic-lens or generic-optics with 'queryable' instead"  #-}

-- | The type of the configuration property.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acpType :: Lens.Lens' ActionConfigurationProperty (Core.Maybe Types.ActionConfigurationPropertyType)
acpType = Lens.field @"type'"
{-# INLINEABLE acpType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON ActionConfigurationProperty where
        toJSON ActionConfigurationProperty{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  Core.Just ("required" Core..= required),
                  Core.Just ("key" Core..= key), Core.Just ("secret" Core..= secret),
                  ("description" Core..=) Core.<$> description,
                  ("queryable" Core..=) Core.<$> queryable,
                  ("type" Core..=) Core.<$> type'])

instance Core.FromJSON ActionConfigurationProperty where
        parseJSON
          = Core.withObject "ActionConfigurationProperty" Core.$
              \ x ->
                ActionConfigurationProperty' Core.<$>
                  (x Core..: "name") Core.<*> x Core..: "required" Core.<*>
                    x Core..: "key"
                    Core.<*> x Core..: "secret"
                    Core.<*> x Core..:? "description"
                    Core.<*> x Core..:? "queryable"
                    Core.<*> x Core..:? "type"
