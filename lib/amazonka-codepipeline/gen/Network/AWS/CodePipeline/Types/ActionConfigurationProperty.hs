{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionConfigurationProperty
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionConfigurationProperty
  ( ActionConfigurationProperty (..),

    -- * Smart constructor
    mkActionConfigurationProperty,

    -- * Lenses
    acpQueryable,
    acpSecret,
    acpRequired,
    acpKey,
    acpName,
    acpType,
    acpDescription,
  )
where

import Network.AWS.CodePipeline.Types.ActionConfigurationPropertyType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents information about an action configuration property.
--
-- /See:/ 'mkActionConfigurationProperty' smart constructor.
data ActionConfigurationProperty = ActionConfigurationProperty'
  { -- | Indicates that the property is used with @PollForJobs@ . When creating a custom action, an action can have up to one queryable property. If it has one, that property must be both required and not secret.
    --
    -- If you create a pipeline with a custom action type, and that custom action contains a queryable property, the value for that configuration property is subject to other restrictions. The value must be less than or equal to twenty (20) characters. The value can contain only alphanumeric characters, underscores, and hyphens.
    queryable :: Lude.Maybe Lude.Bool,
    -- | Whether the configuration property is secret. Secrets are hidden from all calls except for @GetJobDetails@ , @GetThirdPartyJobDetails@ , @PollForJobs@ , and @PollForThirdPartyJobs@ .
    --
    -- When updating a pipeline, passing * * * * * without changing any other values of the action preserves the previous value of the secret.
    secret :: Lude.Bool,
    -- | Whether the configuration property is a required value.
    required :: Lude.Bool,
    -- | Whether the configuration property is a key.
    key :: Lude.Bool,
    -- | The name of the action configuration property.
    name :: Lude.Text,
    -- | The type of the configuration property.
    type' :: Lude.Maybe ActionConfigurationPropertyType,
    -- | The description of the action configuration property that is displayed to users.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActionConfigurationProperty' with the minimum fields required to make a request.
--
-- * 'queryable' - Indicates that the property is used with @PollForJobs@ . When creating a custom action, an action can have up to one queryable property. If it has one, that property must be both required and not secret.
--
-- If you create a pipeline with a custom action type, and that custom action contains a queryable property, the value for that configuration property is subject to other restrictions. The value must be less than or equal to twenty (20) characters. The value can contain only alphanumeric characters, underscores, and hyphens.
-- * 'secret' - Whether the configuration property is secret. Secrets are hidden from all calls except for @GetJobDetails@ , @GetThirdPartyJobDetails@ , @PollForJobs@ , and @PollForThirdPartyJobs@ .
--
-- When updating a pipeline, passing * * * * * without changing any other values of the action preserves the previous value of the secret.
-- * 'required' - Whether the configuration property is a required value.
-- * 'key' - Whether the configuration property is a key.
-- * 'name' - The name of the action configuration property.
-- * 'type'' - The type of the configuration property.
-- * 'description' - The description of the action configuration property that is displayed to users.
mkActionConfigurationProperty ::
  -- | 'secret'
  Lude.Bool ->
  -- | 'required'
  Lude.Bool ->
  -- | 'key'
  Lude.Bool ->
  -- | 'name'
  Lude.Text ->
  ActionConfigurationProperty
mkActionConfigurationProperty pSecret_ pRequired_ pKey_ pName_ =
  ActionConfigurationProperty'
    { queryable = Lude.Nothing,
      secret = pSecret_,
      required = pRequired_,
      key = pKey_,
      name = pName_,
      type' = Lude.Nothing,
      description = Lude.Nothing
    }

-- | Indicates that the property is used with @PollForJobs@ . When creating a custom action, an action can have up to one queryable property. If it has one, that property must be both required and not secret.
--
-- If you create a pipeline with a custom action type, and that custom action contains a queryable property, the value for that configuration property is subject to other restrictions. The value must be less than or equal to twenty (20) characters. The value can contain only alphanumeric characters, underscores, and hyphens.
--
-- /Note:/ Consider using 'queryable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acpQueryable :: Lens.Lens' ActionConfigurationProperty (Lude.Maybe Lude.Bool)
acpQueryable = Lens.lens (queryable :: ActionConfigurationProperty -> Lude.Maybe Lude.Bool) (\s a -> s {queryable = a} :: ActionConfigurationProperty)
{-# DEPRECATED acpQueryable "Use generic-lens or generic-optics with 'queryable' instead." #-}

-- | Whether the configuration property is secret. Secrets are hidden from all calls except for @GetJobDetails@ , @GetThirdPartyJobDetails@ , @PollForJobs@ , and @PollForThirdPartyJobs@ .
--
-- When updating a pipeline, passing * * * * * without changing any other values of the action preserves the previous value of the secret.
--
-- /Note:/ Consider using 'secret' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acpSecret :: Lens.Lens' ActionConfigurationProperty Lude.Bool
acpSecret = Lens.lens (secret :: ActionConfigurationProperty -> Lude.Bool) (\s a -> s {secret = a} :: ActionConfigurationProperty)
{-# DEPRECATED acpSecret "Use generic-lens or generic-optics with 'secret' instead." #-}

-- | Whether the configuration property is a required value.
--
-- /Note:/ Consider using 'required' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acpRequired :: Lens.Lens' ActionConfigurationProperty Lude.Bool
acpRequired = Lens.lens (required :: ActionConfigurationProperty -> Lude.Bool) (\s a -> s {required = a} :: ActionConfigurationProperty)
{-# DEPRECATED acpRequired "Use generic-lens or generic-optics with 'required' instead." #-}

-- | Whether the configuration property is a key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acpKey :: Lens.Lens' ActionConfigurationProperty Lude.Bool
acpKey = Lens.lens (key :: ActionConfigurationProperty -> Lude.Bool) (\s a -> s {key = a} :: ActionConfigurationProperty)
{-# DEPRECATED acpKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The name of the action configuration property.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acpName :: Lens.Lens' ActionConfigurationProperty Lude.Text
acpName = Lens.lens (name :: ActionConfigurationProperty -> Lude.Text) (\s a -> s {name = a} :: ActionConfigurationProperty)
{-# DEPRECATED acpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of the configuration property.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acpType :: Lens.Lens' ActionConfigurationProperty (Lude.Maybe ActionConfigurationPropertyType)
acpType = Lens.lens (type' :: ActionConfigurationProperty -> Lude.Maybe ActionConfigurationPropertyType) (\s a -> s {type' = a} :: ActionConfigurationProperty)
{-# DEPRECATED acpType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The description of the action configuration property that is displayed to users.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acpDescription :: Lens.Lens' ActionConfigurationProperty (Lude.Maybe Lude.Text)
acpDescription = Lens.lens (description :: ActionConfigurationProperty -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ActionConfigurationProperty)
{-# DEPRECATED acpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON ActionConfigurationProperty where
  parseJSON =
    Lude.withObject
      "ActionConfigurationProperty"
      ( \x ->
          ActionConfigurationProperty'
            Lude.<$> (x Lude..:? "queryable")
            Lude.<*> (x Lude..: "secret")
            Lude.<*> (x Lude..: "required")
            Lude.<*> (x Lude..: "key")
            Lude.<*> (x Lude..: "name")
            Lude.<*> (x Lude..:? "type")
            Lude.<*> (x Lude..:? "description")
      )

instance Lude.ToJSON ActionConfigurationProperty where
  toJSON ActionConfigurationProperty' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("queryable" Lude..=) Lude.<$> queryable,
            Lude.Just ("secret" Lude..= secret),
            Lude.Just ("required" Lude..= required),
            Lude.Just ("key" Lude..= key),
            Lude.Just ("name" Lude..= name),
            ("type" Lude..=) Lude.<$> type',
            ("description" Lude..=) Lude.<$> description
          ]
      )
