{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.SDKConfigurationProperty
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.SDKConfigurationProperty
  ( SDKConfigurationProperty (..),

    -- * Smart constructor
    mkSDKConfigurationProperty,

    -- * Lenses
    scpFriendlyName,
    scpRequired,
    scpName,
    scpDefaultValue,
    scpDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A configuration property of an SDK type.
--
-- /See:/ 'mkSDKConfigurationProperty' smart constructor.
data SDKConfigurationProperty = SDKConfigurationProperty'
  { -- | The user-friendly name of an 'SdkType' configuration property.
    friendlyName :: Lude.Maybe Lude.Text,
    -- | A boolean flag of an 'SdkType' configuration property to indicate if the associated SDK configuration property is required (@true@ ) or not (@false@ ).
    required :: Lude.Maybe Lude.Bool,
    -- | The name of a an 'SdkType' configuration property.
    name :: Lude.Maybe Lude.Text,
    -- | The default value of an 'SdkType' configuration property.
    defaultValue :: Lude.Maybe Lude.Text,
    -- | The description of an 'SdkType' configuration property.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SDKConfigurationProperty' with the minimum fields required to make a request.
--
-- * 'friendlyName' - The user-friendly name of an 'SdkType' configuration property.
-- * 'required' - A boolean flag of an 'SdkType' configuration property to indicate if the associated SDK configuration property is required (@true@ ) or not (@false@ ).
-- * 'name' - The name of a an 'SdkType' configuration property.
-- * 'defaultValue' - The default value of an 'SdkType' configuration property.
-- * 'description' - The description of an 'SdkType' configuration property.
mkSDKConfigurationProperty ::
  SDKConfigurationProperty
mkSDKConfigurationProperty =
  SDKConfigurationProperty'
    { friendlyName = Lude.Nothing,
      required = Lude.Nothing,
      name = Lude.Nothing,
      defaultValue = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The user-friendly name of an 'SdkType' configuration property.
--
-- /Note:/ Consider using 'friendlyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scpFriendlyName :: Lens.Lens' SDKConfigurationProperty (Lude.Maybe Lude.Text)
scpFriendlyName = Lens.lens (friendlyName :: SDKConfigurationProperty -> Lude.Maybe Lude.Text) (\s a -> s {friendlyName = a} :: SDKConfigurationProperty)
{-# DEPRECATED scpFriendlyName "Use generic-lens or generic-optics with 'friendlyName' instead." #-}

-- | A boolean flag of an 'SdkType' configuration property to indicate if the associated SDK configuration property is required (@true@ ) or not (@false@ ).
--
-- /Note:/ Consider using 'required' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scpRequired :: Lens.Lens' SDKConfigurationProperty (Lude.Maybe Lude.Bool)
scpRequired = Lens.lens (required :: SDKConfigurationProperty -> Lude.Maybe Lude.Bool) (\s a -> s {required = a} :: SDKConfigurationProperty)
{-# DEPRECATED scpRequired "Use generic-lens or generic-optics with 'required' instead." #-}

-- | The name of a an 'SdkType' configuration property.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scpName :: Lens.Lens' SDKConfigurationProperty (Lude.Maybe Lude.Text)
scpName = Lens.lens (name :: SDKConfigurationProperty -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: SDKConfigurationProperty)
{-# DEPRECATED scpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The default value of an 'SdkType' configuration property.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scpDefaultValue :: Lens.Lens' SDKConfigurationProperty (Lude.Maybe Lude.Text)
scpDefaultValue = Lens.lens (defaultValue :: SDKConfigurationProperty -> Lude.Maybe Lude.Text) (\s a -> s {defaultValue = a} :: SDKConfigurationProperty)
{-# DEPRECATED scpDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

-- | The description of an 'SdkType' configuration property.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scpDescription :: Lens.Lens' SDKConfigurationProperty (Lude.Maybe Lude.Text)
scpDescription = Lens.lens (description :: SDKConfigurationProperty -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: SDKConfigurationProperty)
{-# DEPRECATED scpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON SDKConfigurationProperty where
  parseJSON =
    Lude.withObject
      "SDKConfigurationProperty"
      ( \x ->
          SDKConfigurationProperty'
            Lude.<$> (x Lude..:? "friendlyName")
            Lude.<*> (x Lude..:? "required")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "defaultValue")
            Lude.<*> (x Lude..:? "description")
      )
