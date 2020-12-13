{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ServerValidationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ServerValidationConfiguration
  ( ServerValidationConfiguration (..),

    -- * Smart constructor
    mkServerValidationConfiguration,

    -- * Lenses
    svcServerValidationStrategy,
    svcUserDataValidationParameters,
    svcName,
    svcServer,
    svcValidationId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SMS.Types.Server
import Network.AWS.SMS.Types.ServerValidationStrategy
import Network.AWS.SMS.Types.UserDataValidationParameters

-- | Configuration for validating an instance.
--
-- /See:/ 'mkServerValidationConfiguration' smart constructor.
data ServerValidationConfiguration = ServerValidationConfiguration'
  { -- | The validation strategy.
    serverValidationStrategy :: Lude.Maybe ServerValidationStrategy,
    -- | The validation parameters.
    userDataValidationParameters :: Lude.Maybe UserDataValidationParameters,
    -- | The name of the configuration.
    name :: Lude.Maybe Lude.Text,
    server :: Lude.Maybe Server,
    -- | The ID of the validation.
    validationId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServerValidationConfiguration' with the minimum fields required to make a request.
--
-- * 'serverValidationStrategy' - The validation strategy.
-- * 'userDataValidationParameters' - The validation parameters.
-- * 'name' - The name of the configuration.
-- * 'server' -
-- * 'validationId' - The ID of the validation.
mkServerValidationConfiguration ::
  ServerValidationConfiguration
mkServerValidationConfiguration =
  ServerValidationConfiguration'
    { serverValidationStrategy =
        Lude.Nothing,
      userDataValidationParameters = Lude.Nothing,
      name = Lude.Nothing,
      server = Lude.Nothing,
      validationId = Lude.Nothing
    }

-- | The validation strategy.
--
-- /Note:/ Consider using 'serverValidationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svcServerValidationStrategy :: Lens.Lens' ServerValidationConfiguration (Lude.Maybe ServerValidationStrategy)
svcServerValidationStrategy = Lens.lens (serverValidationStrategy :: ServerValidationConfiguration -> Lude.Maybe ServerValidationStrategy) (\s a -> s {serverValidationStrategy = a} :: ServerValidationConfiguration)
{-# DEPRECATED svcServerValidationStrategy "Use generic-lens or generic-optics with 'serverValidationStrategy' instead." #-}

-- | The validation parameters.
--
-- /Note:/ Consider using 'userDataValidationParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svcUserDataValidationParameters :: Lens.Lens' ServerValidationConfiguration (Lude.Maybe UserDataValidationParameters)
svcUserDataValidationParameters = Lens.lens (userDataValidationParameters :: ServerValidationConfiguration -> Lude.Maybe UserDataValidationParameters) (\s a -> s {userDataValidationParameters = a} :: ServerValidationConfiguration)
{-# DEPRECATED svcUserDataValidationParameters "Use generic-lens or generic-optics with 'userDataValidationParameters' instead." #-}

-- | The name of the configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svcName :: Lens.Lens' ServerValidationConfiguration (Lude.Maybe Lude.Text)
svcName = Lens.lens (name :: ServerValidationConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ServerValidationConfiguration)
{-# DEPRECATED svcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'server' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svcServer :: Lens.Lens' ServerValidationConfiguration (Lude.Maybe Server)
svcServer = Lens.lens (server :: ServerValidationConfiguration -> Lude.Maybe Server) (\s a -> s {server = a} :: ServerValidationConfiguration)
{-# DEPRECATED svcServer "Use generic-lens or generic-optics with 'server' instead." #-}

-- | The ID of the validation.
--
-- /Note:/ Consider using 'validationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svcValidationId :: Lens.Lens' ServerValidationConfiguration (Lude.Maybe Lude.Text)
svcValidationId = Lens.lens (validationId :: ServerValidationConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {validationId = a} :: ServerValidationConfiguration)
{-# DEPRECATED svcValidationId "Use generic-lens or generic-optics with 'validationId' instead." #-}

instance Lude.FromJSON ServerValidationConfiguration where
  parseJSON =
    Lude.withObject
      "ServerValidationConfiguration"
      ( \x ->
          ServerValidationConfiguration'
            Lude.<$> (x Lude..:? "serverValidationStrategy")
            Lude.<*> (x Lude..:? "userDataValidationParameters")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "server")
            Lude.<*> (x Lude..:? "validationId")
      )

instance Lude.ToJSON ServerValidationConfiguration where
  toJSON ServerValidationConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("serverValidationStrategy" Lude..=)
              Lude.<$> serverValidationStrategy,
            ("userDataValidationParameters" Lude..=)
              Lude.<$> userDataValidationParameters,
            ("name" Lude..=) Lude.<$> name,
            ("server" Lude..=) Lude.<$> server,
            ("validationId" Lude..=) Lude.<$> validationId
          ]
      )
