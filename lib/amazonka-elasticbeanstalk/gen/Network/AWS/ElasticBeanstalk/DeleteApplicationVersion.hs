{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DeleteApplicationVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified version from the specified application.
module Network.AWS.ElasticBeanstalk.DeleteApplicationVersion
    (
    -- * Creating a request
      DeleteApplicationVersion (..)
    , mkDeleteApplicationVersion
    -- ** Request lenses
    , davApplicationName
    , davVersionLabel
    , davDeleteSourceBundle

    -- * Destructuring the response
    , DeleteApplicationVersionResponse (..)
    , mkDeleteApplicationVersionResponse
    ) where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to delete an application version.
--
-- /See:/ 'mkDeleteApplicationVersion' smart constructor.
data DeleteApplicationVersion = DeleteApplicationVersion'
  { applicationName :: Types.ApplicationName
    -- ^ The name of the application to which the version belongs.
  , versionLabel :: Types.VersionLabel
    -- ^ The label of the version to delete.
  , deleteSourceBundle :: Core.Maybe Core.Bool
    -- ^ Set to @true@ to delete the source bundle from your storage bucket. Otherwise, the application version is deleted only from Elastic Beanstalk and the source bundle remains in Amazon S3.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApplicationVersion' value with any optional fields omitted.
mkDeleteApplicationVersion
    :: Types.ApplicationName -- ^ 'applicationName'
    -> Types.VersionLabel -- ^ 'versionLabel'
    -> DeleteApplicationVersion
mkDeleteApplicationVersion applicationName versionLabel
  = DeleteApplicationVersion'{applicationName, versionLabel,
                              deleteSourceBundle = Core.Nothing}

-- | The name of the application to which the version belongs.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davApplicationName :: Lens.Lens' DeleteApplicationVersion Types.ApplicationName
davApplicationName = Lens.field @"applicationName"
{-# INLINEABLE davApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | The label of the version to delete.
--
-- /Note:/ Consider using 'versionLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davVersionLabel :: Lens.Lens' DeleteApplicationVersion Types.VersionLabel
davVersionLabel = Lens.field @"versionLabel"
{-# INLINEABLE davVersionLabel #-}
{-# DEPRECATED versionLabel "Use generic-lens or generic-optics with 'versionLabel' instead"  #-}

-- | Set to @true@ to delete the source bundle from your storage bucket. Otherwise, the application version is deleted only from Elastic Beanstalk and the source bundle remains in Amazon S3.
--
-- /Note:/ Consider using 'deleteSourceBundle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davDeleteSourceBundle :: Lens.Lens' DeleteApplicationVersion (Core.Maybe Core.Bool)
davDeleteSourceBundle = Lens.field @"deleteSourceBundle"
{-# INLINEABLE davDeleteSourceBundle #-}
{-# DEPRECATED deleteSourceBundle "Use generic-lens or generic-optics with 'deleteSourceBundle' instead"  #-}

instance Core.ToQuery DeleteApplicationVersion where
        toQuery DeleteApplicationVersion{..}
          = Core.toQueryPair "Action"
              ("DeleteApplicationVersion" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ApplicationName" applicationName
              Core.<> Core.toQueryPair "VersionLabel" versionLabel
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DeleteSourceBundle")
                deleteSourceBundle

instance Core.ToHeaders DeleteApplicationVersion where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteApplicationVersion where
        type Rs DeleteApplicationVersion = DeleteApplicationVersionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeleteApplicationVersionResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteApplicationVersionResponse' smart constructor.
data DeleteApplicationVersionResponse = DeleteApplicationVersionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApplicationVersionResponse' value with any optional fields omitted.
mkDeleteApplicationVersionResponse
    :: DeleteApplicationVersionResponse
mkDeleteApplicationVersionResponse
  = DeleteApplicationVersionResponse'
