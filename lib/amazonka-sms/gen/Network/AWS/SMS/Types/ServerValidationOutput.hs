{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ServerValidationOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ServerValidationOutput
  ( ServerValidationOutput (..),

    -- * Smart constructor
    mkServerValidationOutput,

    -- * Lenses
    svoServer,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SMS.Types.Server as Types

-- | Contains output from validating an instance.
--
-- /See:/ 'mkServerValidationOutput' smart constructor.
newtype ServerValidationOutput = ServerValidationOutput'
  { server :: Core.Maybe Types.Server
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ServerValidationOutput' value with any optional fields omitted.
mkServerValidationOutput ::
  ServerValidationOutput
mkServerValidationOutput =
  ServerValidationOutput' {server = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'server' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svoServer :: Lens.Lens' ServerValidationOutput (Core.Maybe Types.Server)
svoServer = Lens.field @"server"
{-# DEPRECATED svoServer "Use generic-lens or generic-optics with 'server' instead." #-}

instance Core.FromJSON ServerValidationOutput where
  parseJSON =
    Core.withObject "ServerValidationOutput" Core.$
      \x -> ServerValidationOutput' Core.<$> (x Core..:? "server")
