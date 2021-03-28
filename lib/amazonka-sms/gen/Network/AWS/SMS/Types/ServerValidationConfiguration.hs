{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ServerValidationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SMS.Types.ServerValidationConfiguration
  ( ServerValidationConfiguration (..)
  -- * Smart constructor
  , mkServerValidationConfiguration
  -- * Lenses
  , svcName
  , svcServer
  , svcServerValidationStrategy
  , svcUserDataValidationParameters
  , svcValidationId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SMS.Types.NonEmptyStringWithMaxLen255 as Types
import qualified Network.AWS.SMS.Types.Server as Types
import qualified Network.AWS.SMS.Types.ServerValidationStrategy as Types
import qualified Network.AWS.SMS.Types.UserDataValidationParameters as Types
import qualified Network.AWS.SMS.Types.ValidationId as Types

-- | Configuration for validating an instance.
--
-- /See:/ 'mkServerValidationConfiguration' smart constructor.
data ServerValidationConfiguration = ServerValidationConfiguration'
  { name :: Core.Maybe Types.NonEmptyStringWithMaxLen255
    -- ^ The name of the configuration.
  , server :: Core.Maybe Types.Server
  , serverValidationStrategy :: Core.Maybe Types.ServerValidationStrategy
    -- ^ The validation strategy.
  , userDataValidationParameters :: Core.Maybe Types.UserDataValidationParameters
    -- ^ The validation parameters.
  , validationId :: Core.Maybe Types.ValidationId
    -- ^ The ID of the validation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ServerValidationConfiguration' value with any optional fields omitted.
mkServerValidationConfiguration
    :: ServerValidationConfiguration
mkServerValidationConfiguration
  = ServerValidationConfiguration'{name = Core.Nothing,
                                   server = Core.Nothing, serverValidationStrategy = Core.Nothing,
                                   userDataValidationParameters = Core.Nothing,
                                   validationId = Core.Nothing}

-- | The name of the configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svcName :: Lens.Lens' ServerValidationConfiguration (Core.Maybe Types.NonEmptyStringWithMaxLen255)
svcName = Lens.field @"name"
{-# INLINEABLE svcName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'server' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svcServer :: Lens.Lens' ServerValidationConfiguration (Core.Maybe Types.Server)
svcServer = Lens.field @"server"
{-# INLINEABLE svcServer #-}
{-# DEPRECATED server "Use generic-lens or generic-optics with 'server' instead"  #-}

-- | The validation strategy.
--
-- /Note:/ Consider using 'serverValidationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svcServerValidationStrategy :: Lens.Lens' ServerValidationConfiguration (Core.Maybe Types.ServerValidationStrategy)
svcServerValidationStrategy = Lens.field @"serverValidationStrategy"
{-# INLINEABLE svcServerValidationStrategy #-}
{-# DEPRECATED serverValidationStrategy "Use generic-lens or generic-optics with 'serverValidationStrategy' instead"  #-}

-- | The validation parameters.
--
-- /Note:/ Consider using 'userDataValidationParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svcUserDataValidationParameters :: Lens.Lens' ServerValidationConfiguration (Core.Maybe Types.UserDataValidationParameters)
svcUserDataValidationParameters = Lens.field @"userDataValidationParameters"
{-# INLINEABLE svcUserDataValidationParameters #-}
{-# DEPRECATED userDataValidationParameters "Use generic-lens or generic-optics with 'userDataValidationParameters' instead"  #-}

-- | The ID of the validation.
--
-- /Note:/ Consider using 'validationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svcValidationId :: Lens.Lens' ServerValidationConfiguration (Core.Maybe Types.ValidationId)
svcValidationId = Lens.field @"validationId"
{-# INLINEABLE svcValidationId #-}
{-# DEPRECATED validationId "Use generic-lens or generic-optics with 'validationId' instead"  #-}

instance Core.FromJSON ServerValidationConfiguration where
        toJSON ServerValidationConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("name" Core..=) Core.<$> name,
                  ("server" Core..=) Core.<$> server,
                  ("serverValidationStrategy" Core..=) Core.<$>
                    serverValidationStrategy,
                  ("userDataValidationParameters" Core..=) Core.<$>
                    userDataValidationParameters,
                  ("validationId" Core..=) Core.<$> validationId])

instance Core.FromJSON ServerValidationConfiguration where
        parseJSON
          = Core.withObject "ServerValidationConfiguration" Core.$
              \ x ->
                ServerValidationConfiguration' Core.<$>
                  (x Core..:? "name") Core.<*> x Core..:? "server" Core.<*>
                    x Core..:? "serverValidationStrategy"
                    Core.<*> x Core..:? "userDataValidationParameters"
                    Core.<*> x Core..:? "validationId"
