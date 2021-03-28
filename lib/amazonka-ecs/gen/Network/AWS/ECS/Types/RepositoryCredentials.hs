{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.RepositoryCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.RepositoryCredentials
  ( RepositoryCredentials (..)
  -- * Smart constructor
  , mkRepositoryCredentials
  -- * Lenses
  , rcCredentialsParameter
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The repository credentials for private registry authentication.
--
-- /See:/ 'mkRepositoryCredentials' smart constructor.
newtype RepositoryCredentials = RepositoryCredentials'
  { credentialsParameter :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the secret containing the private repository credentials.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RepositoryCredentials' value with any optional fields omitted.
mkRepositoryCredentials
    :: Core.Text -- ^ 'credentialsParameter'
    -> RepositoryCredentials
mkRepositoryCredentials credentialsParameter
  = RepositoryCredentials'{credentialsParameter}

-- | The Amazon Resource Name (ARN) of the secret containing the private repository credentials.
--
-- /Note:/ Consider using 'credentialsParameter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcCredentialsParameter :: Lens.Lens' RepositoryCredentials Core.Text
rcCredentialsParameter = Lens.field @"credentialsParameter"
{-# INLINEABLE rcCredentialsParameter #-}
{-# DEPRECATED credentialsParameter "Use generic-lens or generic-optics with 'credentialsParameter' instead"  #-}

instance Core.FromJSON RepositoryCredentials where
        toJSON RepositoryCredentials{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("credentialsParameter" Core..= credentialsParameter)])

instance Core.FromJSON RepositoryCredentials where
        parseJSON
          = Core.withObject "RepositoryCredentials" Core.$
              \ x ->
                RepositoryCredentials' Core.<$> (x Core..: "credentialsParameter")
