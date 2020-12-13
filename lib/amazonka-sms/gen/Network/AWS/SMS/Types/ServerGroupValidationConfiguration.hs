{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ServerGroupValidationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ServerGroupValidationConfiguration
  ( ServerGroupValidationConfiguration (..),

    -- * Smart constructor
    mkServerGroupValidationConfiguration,

    -- * Lenses
    sgvcServerValidationConfigurations,
    sgvcServerGroupId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SMS.Types.ServerValidationConfiguration

-- | Configuration for validating an instance.
--
-- /See:/ 'mkServerGroupValidationConfiguration' smart constructor.
data ServerGroupValidationConfiguration = ServerGroupValidationConfiguration'
  { -- | The validation configuration.
    serverValidationConfigurations :: Lude.Maybe [ServerValidationConfiguration],
    -- | The ID of the server group.
    serverGroupId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServerGroupValidationConfiguration' with the minimum fields required to make a request.
--
-- * 'serverValidationConfigurations' - The validation configuration.
-- * 'serverGroupId' - The ID of the server group.
mkServerGroupValidationConfiguration ::
  ServerGroupValidationConfiguration
mkServerGroupValidationConfiguration =
  ServerGroupValidationConfiguration'
    { serverValidationConfigurations =
        Lude.Nothing,
      serverGroupId = Lude.Nothing
    }

-- | The validation configuration.
--
-- /Note:/ Consider using 'serverValidationConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgvcServerValidationConfigurations :: Lens.Lens' ServerGroupValidationConfiguration (Lude.Maybe [ServerValidationConfiguration])
sgvcServerValidationConfigurations = Lens.lens (serverValidationConfigurations :: ServerGroupValidationConfiguration -> Lude.Maybe [ServerValidationConfiguration]) (\s a -> s {serverValidationConfigurations = a} :: ServerGroupValidationConfiguration)
{-# DEPRECATED sgvcServerValidationConfigurations "Use generic-lens or generic-optics with 'serverValidationConfigurations' instead." #-}

-- | The ID of the server group.
--
-- /Note:/ Consider using 'serverGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgvcServerGroupId :: Lens.Lens' ServerGroupValidationConfiguration (Lude.Maybe Lude.Text)
sgvcServerGroupId = Lens.lens (serverGroupId :: ServerGroupValidationConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {serverGroupId = a} :: ServerGroupValidationConfiguration)
{-# DEPRECATED sgvcServerGroupId "Use generic-lens or generic-optics with 'serverGroupId' instead." #-}

instance Lude.FromJSON ServerGroupValidationConfiguration where
  parseJSON =
    Lude.withObject
      "ServerGroupValidationConfiguration"
      ( \x ->
          ServerGroupValidationConfiguration'
            Lude.<$> (x Lude..:? "serverValidationConfigurations" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "serverGroupId")
      )

instance Lude.ToJSON ServerGroupValidationConfiguration where
  toJSON ServerGroupValidationConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("serverValidationConfigurations" Lude..=)
              Lude.<$> serverValidationConfigurations,
            ("serverGroupId" Lude..=) Lude.<$> serverGroupId
          ]
      )
