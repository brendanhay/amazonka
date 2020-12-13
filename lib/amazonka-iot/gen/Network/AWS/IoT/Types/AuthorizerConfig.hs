{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuthorizerConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuthorizerConfig
  ( AuthorizerConfig (..),

    -- * Smart constructor
    mkAuthorizerConfig,

    -- * Lenses
    acAllowAuthorizerOverride,
    acDefaultAuthorizerName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object that specifies the authorization service for a domain.
--
-- /See:/ 'mkAuthorizerConfig' smart constructor.
data AuthorizerConfig = AuthorizerConfig'
  { -- | A Boolean that specifies whether the domain configuration's authorization service can be overridden.
    allowAuthorizerOverride :: Lude.Maybe Lude.Bool,
    -- | The name of the authorization service for a domain configuration.
    defaultAuthorizerName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AuthorizerConfig' with the minimum fields required to make a request.
--
-- * 'allowAuthorizerOverride' - A Boolean that specifies whether the domain configuration's authorization service can be overridden.
-- * 'defaultAuthorizerName' - The name of the authorization service for a domain configuration.
mkAuthorizerConfig ::
  AuthorizerConfig
mkAuthorizerConfig =
  AuthorizerConfig'
    { allowAuthorizerOverride = Lude.Nothing,
      defaultAuthorizerName = Lude.Nothing
    }

-- | A Boolean that specifies whether the domain configuration's authorization service can be overridden.
--
-- /Note:/ Consider using 'allowAuthorizerOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acAllowAuthorizerOverride :: Lens.Lens' AuthorizerConfig (Lude.Maybe Lude.Bool)
acAllowAuthorizerOverride = Lens.lens (allowAuthorizerOverride :: AuthorizerConfig -> Lude.Maybe Lude.Bool) (\s a -> s {allowAuthorizerOverride = a} :: AuthorizerConfig)
{-# DEPRECATED acAllowAuthorizerOverride "Use generic-lens or generic-optics with 'allowAuthorizerOverride' instead." #-}

-- | The name of the authorization service for a domain configuration.
--
-- /Note:/ Consider using 'defaultAuthorizerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acDefaultAuthorizerName :: Lens.Lens' AuthorizerConfig (Lude.Maybe Lude.Text)
acDefaultAuthorizerName = Lens.lens (defaultAuthorizerName :: AuthorizerConfig -> Lude.Maybe Lude.Text) (\s a -> s {defaultAuthorizerName = a} :: AuthorizerConfig)
{-# DEPRECATED acDefaultAuthorizerName "Use generic-lens or generic-optics with 'defaultAuthorizerName' instead." #-}

instance Lude.FromJSON AuthorizerConfig where
  parseJSON =
    Lude.withObject
      "AuthorizerConfig"
      ( \x ->
          AuthorizerConfig'
            Lude.<$> (x Lude..:? "allowAuthorizerOverride")
            Lude.<*> (x Lude..:? "defaultAuthorizerName")
      )

instance Lude.ToJSON AuthorizerConfig where
  toJSON AuthorizerConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("allowAuthorizerOverride" Lude..=)
              Lude.<$> allowAuthorizerOverride,
            ("defaultAuthorizerName" Lude..=) Lude.<$> defaultAuthorizerName
          ]
      )
