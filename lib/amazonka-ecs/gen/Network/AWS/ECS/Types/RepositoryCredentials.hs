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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The repository credentials for private registry authentication.
--
-- /See:/ 'mkRepositoryCredentials' smart constructor.
newtype RepositoryCredentials = RepositoryCredentials'
  { -- | The Amazon Resource Name (ARN) of the secret containing the private repository credentials.
    credentialsParameter :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RepositoryCredentials' with the minimum fields required to make a request.
--
-- * 'credentialsParameter' - The Amazon Resource Name (ARN) of the secret containing the private repository credentials.
mkRepositoryCredentials ::
  -- | 'credentialsParameter'
  Lude.Text ->
  RepositoryCredentials
mkRepositoryCredentials pCredentialsParameter_ =
  RepositoryCredentials'
    { credentialsParameter =
        pCredentialsParameter_
    }

-- | The Amazon Resource Name (ARN) of the secret containing the private repository credentials.
--
-- /Note:/ Consider using 'credentialsParameter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcCredentialsParameter :: Lens.Lens' RepositoryCredentials Lude.Text
rcCredentialsParameter = Lens.lens (credentialsParameter :: RepositoryCredentials -> Lude.Text) (\s a -> s {credentialsParameter = a} :: RepositoryCredentials)
{-# DEPRECATED rcCredentialsParameter "Use generic-lens or generic-optics with 'credentialsParameter' instead." #-}

instance Lude.FromJSON RepositoryCredentials where
  parseJSON =
    Lude.withObject
      "RepositoryCredentials"
      ( \x ->
          RepositoryCredentials' Lude.<$> (x Lude..: "credentialsParameter")
      )

instance Lude.ToJSON RepositoryCredentials where
  toJSON RepositoryCredentials' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("credentialsParameter" Lude..= credentialsParameter)]
      )
