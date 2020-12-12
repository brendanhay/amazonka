{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.SDKType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.SDKType
  ( SDKType (..),

    -- * Smart constructor
    mkSDKType,

    -- * Lenses
    stFriendlyName,
    stConfigurationProperties,
    stId,
    stDescription,
  )
where

import Network.AWS.APIGateway.Types.SDKConfigurationProperty
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A type of SDK that API Gateway can generate.
--
-- /See:/ 'mkSDKType' smart constructor.
data SDKType = SDKType'
  { friendlyName :: Lude.Maybe Lude.Text,
    configurationProperties :: Lude.Maybe [SDKConfigurationProperty],
    id :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SDKType' with the minimum fields required to make a request.
--
-- * 'configurationProperties' - A list of configuration properties of an 'SdkType' .
-- * 'description' - The description of an 'SdkType' .
-- * 'friendlyName' - The user-friendly name of an 'SdkType' instance.
-- * 'id' - The identifier of an 'SdkType' instance.
mkSDKType ::
  SDKType
mkSDKType =
  SDKType'
    { friendlyName = Lude.Nothing,
      configurationProperties = Lude.Nothing,
      id = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The user-friendly name of an 'SdkType' instance.
--
-- /Note:/ Consider using 'friendlyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stFriendlyName :: Lens.Lens' SDKType (Lude.Maybe Lude.Text)
stFriendlyName = Lens.lens (friendlyName :: SDKType -> Lude.Maybe Lude.Text) (\s a -> s {friendlyName = a} :: SDKType)
{-# DEPRECATED stFriendlyName "Use generic-lens or generic-optics with 'friendlyName' instead." #-}

-- | A list of configuration properties of an 'SdkType' .
--
-- /Note:/ Consider using 'configurationProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stConfigurationProperties :: Lens.Lens' SDKType (Lude.Maybe [SDKConfigurationProperty])
stConfigurationProperties = Lens.lens (configurationProperties :: SDKType -> Lude.Maybe [SDKConfigurationProperty]) (\s a -> s {configurationProperties = a} :: SDKType)
{-# DEPRECATED stConfigurationProperties "Use generic-lens or generic-optics with 'configurationProperties' instead." #-}

-- | The identifier of an 'SdkType' instance.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stId :: Lens.Lens' SDKType (Lude.Maybe Lude.Text)
stId = Lens.lens (id :: SDKType -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: SDKType)
{-# DEPRECATED stId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The description of an 'SdkType' .
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stDescription :: Lens.Lens' SDKType (Lude.Maybe Lude.Text)
stDescription = Lens.lens (description :: SDKType -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: SDKType)
{-# DEPRECATED stDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON SDKType where
  parseJSON =
    Lude.withObject
      "SDKType"
      ( \x ->
          SDKType'
            Lude.<$> (x Lude..:? "friendlyName")
            Lude.<*> (x Lude..:? "configurationProperties" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "description")
      )
