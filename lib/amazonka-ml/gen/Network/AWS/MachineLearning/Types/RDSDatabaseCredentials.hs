{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
import qualified Network.AWS.MachineLearning.Types.RDSDatabasePassword as Types
import qualified Network.AWS.MachineLearning.Types.Username as Types
import qualified Network.AWS.Prelude as Core

-- | The database credentials to connect to a database on an RDS DB instance.
--
-- /See:/ 'mkRDSDatabaseCredentials' smart constructor.
data RDSDatabaseCredentials = RDSDatabaseCredentials'
  { username :: Types.Username,
    password :: Types.RDSDatabasePassword
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RDSDatabaseCredentials' value with any optional fields omitted.
mkRDSDatabaseCredentials ::
  -- | 'username'
  Types.Username ->
  -- | 'password'
  Types.RDSDatabasePassword ->
  RDSDatabaseCredentials
mkRDSDatabaseCredentials username password =
  RDSDatabaseCredentials' {username, password}

-- | Undocumented field.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsdcUsername :: Lens.Lens' RDSDatabaseCredentials Types.Username
rdsdcUsername = Lens.field @"username"
{-# DEPRECATED rdsdcUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsdcPassword :: Lens.Lens' RDSDatabaseCredentials Types.RDSDatabasePassword
rdsdcPassword = Lens.field @"password"
{-# DEPRECATED rdsdcPassword "Use generic-lens or generic-optics with 'password' instead." #-}

instance Core.FromJSON RDSDatabaseCredentials where
  toJSON RDSDatabaseCredentials {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Username" Core..= username),
            Core.Just ("Password" Core..= password)
          ]
      )
