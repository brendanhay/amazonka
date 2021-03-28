{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.UpgradeAppliedSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Upgrades a single directory in-place using the @PublishedSchemaArn@ with schema updates found in @MinorVersion@ . Backwards-compatible minor version upgrades are instantaneously available for readers on all objects in the directory. Note: This is a synchronous API call and upgrades only one schema on a given directory per call. To upgrade multiple directories from one schema, you would need to call this API on each directory.
module Network.AWS.CloudDirectory.UpgradeAppliedSchema
    (
    -- * Creating a request
      UpgradeAppliedSchema (..)
    , mkUpgradeAppliedSchema
    -- ** Request lenses
    , uasPublishedSchemaArn
    , uasDirectoryArn
    , uasDryRun

    -- * Destructuring the response
    , UpgradeAppliedSchemaResponse (..)
    , mkUpgradeAppliedSchemaResponse
    -- ** Response lenses
    , uasrrsDirectoryArn
    , uasrrsUpgradedSchemaArn
    , uasrrsResponseStatus
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpgradeAppliedSchema' smart constructor.
data UpgradeAppliedSchema = UpgradeAppliedSchema'
  { publishedSchemaArn :: Types.Arn
    -- ^ The revision of the published schema to upgrade the directory to.
  , directoryArn :: Types.Arn
    -- ^ The ARN for the directory to which the upgraded schema will be applied.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Used for testing whether the major version schemas are backward compatible or not. If schema compatibility fails, an exception would be thrown else the call would succeed but no changes will be saved. This parameter is optional.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpgradeAppliedSchema' value with any optional fields omitted.
mkUpgradeAppliedSchema
    :: Types.Arn -- ^ 'publishedSchemaArn'
    -> Types.Arn -- ^ 'directoryArn'
    -> UpgradeAppliedSchema
mkUpgradeAppliedSchema publishedSchemaArn directoryArn
  = UpgradeAppliedSchema'{publishedSchemaArn, directoryArn,
                          dryRun = Core.Nothing}

-- | The revision of the published schema to upgrade the directory to.
--
-- /Note:/ Consider using 'publishedSchemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasPublishedSchemaArn :: Lens.Lens' UpgradeAppliedSchema Types.Arn
uasPublishedSchemaArn = Lens.field @"publishedSchemaArn"
{-# INLINEABLE uasPublishedSchemaArn #-}
{-# DEPRECATED publishedSchemaArn "Use generic-lens or generic-optics with 'publishedSchemaArn' instead"  #-}

-- | The ARN for the directory to which the upgraded schema will be applied.
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasDirectoryArn :: Lens.Lens' UpgradeAppliedSchema Types.Arn
uasDirectoryArn = Lens.field @"directoryArn"
{-# INLINEABLE uasDirectoryArn #-}
{-# DEPRECATED directoryArn "Use generic-lens or generic-optics with 'directoryArn' instead"  #-}

-- | Used for testing whether the major version schemas are backward compatible or not. If schema compatibility fails, an exception would be thrown else the call would succeed but no changes will be saved. This parameter is optional.
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasDryRun :: Lens.Lens' UpgradeAppliedSchema (Core.Maybe Core.Bool)
uasDryRun = Lens.field @"dryRun"
{-# INLINEABLE uasDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery UpgradeAppliedSchema where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpgradeAppliedSchema where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON UpgradeAppliedSchema where
        toJSON UpgradeAppliedSchema{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("PublishedSchemaArn" Core..= publishedSchemaArn),
                  Core.Just ("DirectoryArn" Core..= directoryArn),
                  ("DryRun" Core..=) Core.<$> dryRun])

instance Core.AWSRequest UpgradeAppliedSchema where
        type Rs UpgradeAppliedSchema = UpgradeAppliedSchemaResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/amazonclouddirectory/2017-01-11/schema/upgradeapplied",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpgradeAppliedSchemaResponse' Core.<$>
                   (x Core..:? "DirectoryArn") Core.<*> x Core..:? "UpgradedSchemaArn"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpgradeAppliedSchemaResponse' smart constructor.
data UpgradeAppliedSchemaResponse = UpgradeAppliedSchemaResponse'
  { directoryArn :: Core.Maybe Types.DirectoryArn
    -- ^ The ARN of the directory that is returned as part of the response.
  , upgradedSchemaArn :: Core.Maybe Types.UpgradedSchemaArn
    -- ^ The ARN of the upgraded schema that is returned as part of the response.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpgradeAppliedSchemaResponse' value with any optional fields omitted.
mkUpgradeAppliedSchemaResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpgradeAppliedSchemaResponse
mkUpgradeAppliedSchemaResponse responseStatus
  = UpgradeAppliedSchemaResponse'{directoryArn = Core.Nothing,
                                  upgradedSchemaArn = Core.Nothing, responseStatus}

-- | The ARN of the directory that is returned as part of the response.
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasrrsDirectoryArn :: Lens.Lens' UpgradeAppliedSchemaResponse (Core.Maybe Types.DirectoryArn)
uasrrsDirectoryArn = Lens.field @"directoryArn"
{-# INLINEABLE uasrrsDirectoryArn #-}
{-# DEPRECATED directoryArn "Use generic-lens or generic-optics with 'directoryArn' instead"  #-}

-- | The ARN of the upgraded schema that is returned as part of the response.
--
-- /Note:/ Consider using 'upgradedSchemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasrrsUpgradedSchemaArn :: Lens.Lens' UpgradeAppliedSchemaResponse (Core.Maybe Types.UpgradedSchemaArn)
uasrrsUpgradedSchemaArn = Lens.field @"upgradedSchemaArn"
{-# INLINEABLE uasrrsUpgradedSchemaArn #-}
{-# DEPRECATED upgradedSchemaArn "Use generic-lens or generic-optics with 'upgradedSchemaArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasrrsResponseStatus :: Lens.Lens' UpgradeAppliedSchemaResponse Core.Int
uasrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uasrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
