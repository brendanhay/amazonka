{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.RedshiftDatabaseCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.RedshiftDatabaseCredentials
  ( RedshiftDatabaseCredentials (..),

    -- * Smart constructor
    mkRedshiftDatabaseCredentials,

    -- * Lenses
    rdcUsername,
    rdcPassword,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the database credentials for connecting to a database on an Amazon Redshift cluster.
--
-- /See:/ 'mkRedshiftDatabaseCredentials' smart constructor.
data RedshiftDatabaseCredentials = RedshiftDatabaseCredentials'
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

-- | Creates a value of 'RedshiftDatabaseCredentials' with the minimum fields required to make a request.
--
-- * 'password' - Undocumented field.
-- * 'username' - Undocumented field.
mkRedshiftDatabaseCredentials ::
  -- | 'username'
  Lude.Text ->
  -- | 'password'
  Lude.Text ->
  RedshiftDatabaseCredentials
mkRedshiftDatabaseCredentials pUsername_ pPassword_ =
  RedshiftDatabaseCredentials'
    { username = pUsername_,
      password = pPassword_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcUsername :: Lens.Lens' RedshiftDatabaseCredentials Lude.Text
rdcUsername = Lens.lens (username :: RedshiftDatabaseCredentials -> Lude.Text) (\s a -> s {username = a} :: RedshiftDatabaseCredentials)
{-# DEPRECATED rdcUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcPassword :: Lens.Lens' RedshiftDatabaseCredentials Lude.Text
rdcPassword = Lens.lens (password :: RedshiftDatabaseCredentials -> Lude.Text) (\s a -> s {password = a} :: RedshiftDatabaseCredentials)
{-# DEPRECATED rdcPassword "Use generic-lens or generic-optics with 'password' instead." #-}

instance Lude.ToJSON RedshiftDatabaseCredentials where
  toJSON RedshiftDatabaseCredentials' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Username" Lude..= username),
            Lude.Just ("Password" Lude..= password)
          ]
      )
