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
import qualified Network.AWS.MachineLearning.Types.Password as Types
import qualified Network.AWS.MachineLearning.Types.RedshiftDatabaseUsername as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the database credentials for connecting to a database on an Amazon Redshift cluster.
--
-- /See:/ 'mkRedshiftDatabaseCredentials' smart constructor.
data RedshiftDatabaseCredentials = RedshiftDatabaseCredentials'
  { username :: Types.RedshiftDatabaseUsername,
    password :: Types.Password
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RedshiftDatabaseCredentials' value with any optional fields omitted.
mkRedshiftDatabaseCredentials ::
  -- | 'username'
  Types.RedshiftDatabaseUsername ->
  -- | 'password'
  Types.Password ->
  RedshiftDatabaseCredentials
mkRedshiftDatabaseCredentials username password =
  RedshiftDatabaseCredentials' {username, password}

-- | Undocumented field.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcUsername :: Lens.Lens' RedshiftDatabaseCredentials Types.RedshiftDatabaseUsername
rdcUsername = Lens.field @"username"
{-# DEPRECATED rdcUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcPassword :: Lens.Lens' RedshiftDatabaseCredentials Types.Password
rdcPassword = Lens.field @"password"
{-# DEPRECATED rdcPassword "Use generic-lens or generic-optics with 'password' instead." #-}

instance Core.FromJSON RedshiftDatabaseCredentials where
  toJSON RedshiftDatabaseCredentials {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Username" Core..= username),
            Core.Just ("Password" Core..= password)
          ]
      )
