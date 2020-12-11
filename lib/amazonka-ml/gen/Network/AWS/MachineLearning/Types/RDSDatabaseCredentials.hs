-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.RDSDatabaseCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.RDSDatabaseCredentials
  ( RDSDatabaseCredentials (..),

    -- * Smart constructor
    mkRDSDatabaseCredentials,

    -- * Lenses
    rdsdcUsername,
    rdsdcPassword,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The database credentials to connect to a database on an RDS DB instance.
--
-- /See:/ 'mkRDSDatabaseCredentials' smart constructor.
data RDSDatabaseCredentials = RDSDatabaseCredentials'
  { username ::
      Lude.Text,
    password :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RDSDatabaseCredentials' with the minimum fields required to make a request.
--
-- * 'password' - Undocumented field.
-- * 'username' - Undocumented field.
mkRDSDatabaseCredentials ::
  -- | 'username'
  Lude.Text ->
  -- | 'password'
  Lude.Text ->
  RDSDatabaseCredentials
mkRDSDatabaseCredentials pUsername_ pPassword_ =
  RDSDatabaseCredentials'
    { username = pUsername_,
      password = pPassword_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsdcUsername :: Lens.Lens' RDSDatabaseCredentials Lude.Text
rdsdcUsername = Lens.lens (username :: RDSDatabaseCredentials -> Lude.Text) (\s a -> s {username = a} :: RDSDatabaseCredentials)
{-# DEPRECATED rdsdcUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsdcPassword :: Lens.Lens' RDSDatabaseCredentials Lude.Text
rdsdcPassword = Lens.lens (password :: RDSDatabaseCredentials -> Lude.Text) (\s a -> s {password = a} :: RDSDatabaseCredentials)
{-# DEPRECATED rdsdcPassword "Use generic-lens or generic-optics with 'password' instead." #-}

instance Lude.ToJSON RDSDatabaseCredentials where
  toJSON RDSDatabaseCredentials' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Username" Lude..= username),
            Lude.Just ("Password" Lude..= password)
          ]
      )
