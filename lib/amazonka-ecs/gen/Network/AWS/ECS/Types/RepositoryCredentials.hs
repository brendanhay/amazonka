{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.RepositoryCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.RepositoryCredentials
  ( RepositoryCredentials (..),

    -- * Smart constructor
    mkRepositoryCredentials,

    -- * Lenses
    rcCredentialsParameter,
  )
where

import qualified Network.AWS.ECS.Types.CredentialsParameter as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The repository credentials for private registry authentication.
--
-- /See:/ 'mkRepositoryCredentials' smart constructor.
newtype RepositoryCredentials = RepositoryCredentials'
  { -- | The Amazon Resource Name (ARN) of the secret containing the private repository credentials.
    credentialsParameter :: Types.CredentialsParameter
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RepositoryCredentials' value with any optional fields omitted.
mkRepositoryCredentials ::
  -- | 'credentialsParameter'
  Types.CredentialsParameter ->
  RepositoryCredentials
mkRepositoryCredentials credentialsParameter =
  RepositoryCredentials' {credentialsParameter}

-- | The Amazon Resource Name (ARN) of the secret containing the private repository credentials.
--
-- /Note:/ Consider using 'credentialsParameter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcCredentialsParameter :: Lens.Lens' RepositoryCredentials Types.CredentialsParameter
rcCredentialsParameter = Lens.field @"credentialsParameter"
{-# DEPRECATED rcCredentialsParameter "Use generic-lens or generic-optics with 'credentialsParameter' instead." #-}

instance Core.FromJSON RepositoryCredentials where
  toJSON RepositoryCredentials {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("credentialsParameter" Core..= credentialsParameter)]
      )

instance Core.FromJSON RepositoryCredentials where
  parseJSON =
    Core.withObject "RepositoryCredentials" Core.$
      \x ->
        RepositoryCredentials' Core.<$> (x Core..: "credentialsParameter")
