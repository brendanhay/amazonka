{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.UpgradePublishedSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Upgrades a published schema under a new minor version revision using the current contents of @DevelopmentSchemaArn@ .
module Network.AWS.CloudDirectory.UpgradePublishedSchema
  ( -- * Creating a request
    UpgradePublishedSchema (..),
    mkUpgradePublishedSchema,

    -- ** Request lenses
    upsDevelopmentSchemaArn,
    upsPublishedSchemaArn,
    upsMinorVersion,
    upsDryRun,

    -- * Destructuring the response
    UpgradePublishedSchemaResponse (..),
    mkUpgradePublishedSchemaResponse,

    -- ** Response lenses
    upsrrsUpgradedSchemaArn,
    upsrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpgradePublishedSchema' smart constructor.
data UpgradePublishedSchema = UpgradePublishedSchema'
  { -- | The ARN of the development schema with the changes used for the upgrade.
    developmentSchemaArn :: Types.Arn,
    -- | The ARN of the published schema to be upgraded.
    publishedSchemaArn :: Types.Arn,
    -- | Identifies the minor version of the published schema that will be created. This parameter is NOT optional.
    minorVersion :: Types.MinorVersion,
    -- | Used for testing whether the Development schema provided is backwards compatible, or not, with the publish schema provided by the user to be upgraded. If schema compatibility fails, an exception would be thrown else the call would succeed. This parameter is optional and defaults to false.
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpgradePublishedSchema' value with any optional fields omitted.
mkUpgradePublishedSchema ::
  -- | 'developmentSchemaArn'
  Types.Arn ->
  -- | 'publishedSchemaArn'
  Types.Arn ->
  -- | 'minorVersion'
  Types.MinorVersion ->
  UpgradePublishedSchema
mkUpgradePublishedSchema
  developmentSchemaArn
  publishedSchemaArn
  minorVersion =
    UpgradePublishedSchema'
      { developmentSchemaArn,
        publishedSchemaArn,
        minorVersion,
        dryRun = Core.Nothing
      }

-- | The ARN of the development schema with the changes used for the upgrade.
--
-- /Note:/ Consider using 'developmentSchemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upsDevelopmentSchemaArn :: Lens.Lens' UpgradePublishedSchema Types.Arn
upsDevelopmentSchemaArn = Lens.field @"developmentSchemaArn"
{-# DEPRECATED upsDevelopmentSchemaArn "Use generic-lens or generic-optics with 'developmentSchemaArn' instead." #-}

-- | The ARN of the published schema to be upgraded.
--
-- /Note:/ Consider using 'publishedSchemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upsPublishedSchemaArn :: Lens.Lens' UpgradePublishedSchema Types.Arn
upsPublishedSchemaArn = Lens.field @"publishedSchemaArn"
{-# DEPRECATED upsPublishedSchemaArn "Use generic-lens or generic-optics with 'publishedSchemaArn' instead." #-}

-- | Identifies the minor version of the published schema that will be created. This parameter is NOT optional.
--
-- /Note:/ Consider using 'minorVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upsMinorVersion :: Lens.Lens' UpgradePublishedSchema Types.MinorVersion
upsMinorVersion = Lens.field @"minorVersion"
{-# DEPRECATED upsMinorVersion "Use generic-lens or generic-optics with 'minorVersion' instead." #-}

-- | Used for testing whether the Development schema provided is backwards compatible, or not, with the publish schema provided by the user to be upgraded. If schema compatibility fails, an exception would be thrown else the call would succeed. This parameter is optional and defaults to false.
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upsDryRun :: Lens.Lens' UpgradePublishedSchema (Core.Maybe Core.Bool)
upsDryRun = Lens.field @"dryRun"
{-# DEPRECATED upsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.FromJSON UpgradePublishedSchema where
  toJSON UpgradePublishedSchema {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DevelopmentSchemaArn" Core..= developmentSchemaArn),
            Core.Just ("PublishedSchemaArn" Core..= publishedSchemaArn),
            Core.Just ("MinorVersion" Core..= minorVersion),
            ("DryRun" Core..=) Core.<$> dryRun
          ]
      )

instance Core.AWSRequest UpgradePublishedSchema where
  type Rs UpgradePublishedSchema = UpgradePublishedSchemaResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            "/amazonclouddirectory/2017-01-11/schema/upgradepublished",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpgradePublishedSchemaResponse'
            Core.<$> (x Core..:? "UpgradedSchemaArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpgradePublishedSchemaResponse' smart constructor.
data UpgradePublishedSchemaResponse = UpgradePublishedSchemaResponse'
  { -- | The ARN of the upgraded schema that is returned as part of the response.
    upgradedSchemaArn :: Core.Maybe Types.UpgradedSchemaArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpgradePublishedSchemaResponse' value with any optional fields omitted.
mkUpgradePublishedSchemaResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpgradePublishedSchemaResponse
mkUpgradePublishedSchemaResponse responseStatus =
  UpgradePublishedSchemaResponse'
    { upgradedSchemaArn = Core.Nothing,
      responseStatus
    }

-- | The ARN of the upgraded schema that is returned as part of the response.
--
-- /Note:/ Consider using 'upgradedSchemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upsrrsUpgradedSchemaArn :: Lens.Lens' UpgradePublishedSchemaResponse (Core.Maybe Types.UpgradedSchemaArn)
upsrrsUpgradedSchemaArn = Lens.field @"upgradedSchemaArn"
{-# DEPRECATED upsrrsUpgradedSchemaArn "Use generic-lens or generic-optics with 'upgradedSchemaArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upsrrsResponseStatus :: Lens.Lens' UpgradePublishedSchemaResponse Core.Int
upsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED upsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
