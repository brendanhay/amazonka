{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.Configuration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MQ.Types.Configuration
  ( Configuration (..)
  -- * Smart constructor
  , mkConfiguration
  -- * Lenses
  , cArn
  , cAuthenticationStrategy
  , cCreated
  , cDescription
  , cEngineType
  , cEngineVersion
  , cId
  , cLatestRevision
  , cName
  , cTags
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MQ.Types.AuthenticationStrategy as Types
import qualified Network.AWS.MQ.Types.ConfigurationRevision as Types
import qualified Network.AWS.MQ.Types.EngineType as Types
import qualified Network.AWS.Prelude as Core

-- | Returns information about all configurations.
--
-- /See:/ 'mkConfiguration' smart constructor.
data Configuration = Configuration'
  { arn :: Core.Maybe Core.Text
    -- ^ Required. The ARN of the configuration.
  , authenticationStrategy :: Core.Maybe Types.AuthenticationStrategy
    -- ^ The authentication strategy associated with the configuration.
  , created :: Core.Maybe Core.UTCTime
    -- ^ Required. The date and time of the configuration revision.
  , description :: Core.Maybe Core.Text
    -- ^ Required. The description of the configuration.
  , engineType :: Core.Maybe Types.EngineType
    -- ^ Required. The type of broker engine. Note: Currently, Amazon MQ supports ACTIVEMQ and RABBITMQ.
  , engineVersion :: Core.Maybe Core.Text
    -- ^ Required. The version of the broker engine. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
  , id :: Core.Maybe Core.Text
    -- ^ Required. The unique ID that Amazon MQ generates for the configuration.
  , latestRevision :: Core.Maybe Types.ConfigurationRevision
    -- ^ Required. The latest revision of the configuration.
  , name :: Core.Maybe Core.Text
    -- ^ Required. The name of the configuration. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 1-150 characters long.
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The list of all tags associated with this configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Configuration' value with any optional fields omitted.
mkConfiguration
    :: Configuration
mkConfiguration
  = Configuration'{arn = Core.Nothing,
                   authenticationStrategy = Core.Nothing, created = Core.Nothing,
                   description = Core.Nothing, engineType = Core.Nothing,
                   engineVersion = Core.Nothing, id = Core.Nothing,
                   latestRevision = Core.Nothing, name = Core.Nothing,
                   tags = Core.Nothing}

-- | Required. The ARN of the configuration.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cArn :: Lens.Lens' Configuration (Core.Maybe Core.Text)
cArn = Lens.field @"arn"
{-# INLINEABLE cArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The authentication strategy associated with the configuration.
--
-- /Note:/ Consider using 'authenticationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAuthenticationStrategy :: Lens.Lens' Configuration (Core.Maybe Types.AuthenticationStrategy)
cAuthenticationStrategy = Lens.field @"authenticationStrategy"
{-# INLINEABLE cAuthenticationStrategy #-}
{-# DEPRECATED authenticationStrategy "Use generic-lens or generic-optics with 'authenticationStrategy' instead"  #-}

-- | Required. The date and time of the configuration revision.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCreated :: Lens.Lens' Configuration (Core.Maybe Core.UTCTime)
cCreated = Lens.field @"created"
{-# INLINEABLE cCreated #-}
{-# DEPRECATED created "Use generic-lens or generic-optics with 'created' instead"  #-}

-- | Required. The description of the configuration.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDescription :: Lens.Lens' Configuration (Core.Maybe Core.Text)
cDescription = Lens.field @"description"
{-# INLINEABLE cDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Required. The type of broker engine. Note: Currently, Amazon MQ supports ACTIVEMQ and RABBITMQ.
--
-- /Note:/ Consider using 'engineType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cEngineType :: Lens.Lens' Configuration (Core.Maybe Types.EngineType)
cEngineType = Lens.field @"engineType"
{-# INLINEABLE cEngineType #-}
{-# DEPRECATED engineType "Use generic-lens or generic-optics with 'engineType' instead"  #-}

-- | Required. The version of the broker engine. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cEngineVersion :: Lens.Lens' Configuration (Core.Maybe Core.Text)
cEngineVersion = Lens.field @"engineVersion"
{-# INLINEABLE cEngineVersion #-}
{-# DEPRECATED engineVersion "Use generic-lens or generic-optics with 'engineVersion' instead"  #-}

-- | Required. The unique ID that Amazon MQ generates for the configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cId :: Lens.Lens' Configuration (Core.Maybe Core.Text)
cId = Lens.field @"id"
{-# INLINEABLE cId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | Required. The latest revision of the configuration.
--
-- /Note:/ Consider using 'latestRevision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLatestRevision :: Lens.Lens' Configuration (Core.Maybe Types.ConfigurationRevision)
cLatestRevision = Lens.field @"latestRevision"
{-# INLINEABLE cLatestRevision #-}
{-# DEPRECATED latestRevision "Use generic-lens or generic-optics with 'latestRevision' instead"  #-}

-- | Required. The name of the configuration. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 1-150 characters long.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' Configuration (Core.Maybe Core.Text)
cName = Lens.field @"name"
{-# INLINEABLE cName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The list of all tags associated with this configuration.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' Configuration (Core.Maybe (Core.HashMap Core.Text Core.Text))
cTags = Lens.field @"tags"
{-# INLINEABLE cTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON Configuration where
        parseJSON
          = Core.withObject "Configuration" Core.$
              \ x ->
                Configuration' Core.<$>
                  (x Core..:? "arn") Core.<*> x Core..:? "authenticationStrategy"
                    Core.<*> x Core..:? "created"
                    Core.<*> x Core..:? "description"
                    Core.<*> x Core..:? "engineType"
                    Core.<*> x Core..:? "engineVersion"
                    Core.<*> x Core..:? "id"
                    Core.<*> x Core..:? "latestRevision"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "tags"
