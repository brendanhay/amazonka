{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteModelPackage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a model package.
--
-- A model package is used to create Amazon SageMaker models or list on AWS Marketplace. Buyers can subscribe to model packages listed on AWS Marketplace to create models in Amazon SageMaker.
module Network.AWS.SageMaker.DeleteModelPackage
  ( -- * Creating a request
    DeleteModelPackage (..),
    mkDeleteModelPackage,

    -- ** Request lenses
    dmpModelPackageName,

    -- * Destructuring the response
    DeleteModelPackageResponse (..),
    mkDeleteModelPackageResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDeleteModelPackage' smart constructor.
newtype DeleteModelPackage = DeleteModelPackage'
  { -- | The name of the model package. The name must have 1 to 63 characters. Valid characters are a-z, A-Z, 0-9, and - (hyphen).
    modelPackageName :: Types.VersionedArnOrName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteModelPackage' value with any optional fields omitted.
mkDeleteModelPackage ::
  -- | 'modelPackageName'
  Types.VersionedArnOrName ->
  DeleteModelPackage
mkDeleteModelPackage modelPackageName =
  DeleteModelPackage' {modelPackageName}

-- | The name of the model package. The name must have 1 to 63 characters. Valid characters are a-z, A-Z, 0-9, and - (hyphen).
--
-- /Note:/ Consider using 'modelPackageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmpModelPackageName :: Lens.Lens' DeleteModelPackage Types.VersionedArnOrName
dmpModelPackageName = Lens.field @"modelPackageName"
{-# DEPRECATED dmpModelPackageName "Use generic-lens or generic-optics with 'modelPackageName' instead." #-}

instance Core.FromJSON DeleteModelPackage where
  toJSON DeleteModelPackage {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ModelPackageName" Core..= modelPackageName)]
      )

instance Core.AWSRequest DeleteModelPackage where
  type Rs DeleteModelPackage = DeleteModelPackageResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.DeleteModelPackage")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DeleteModelPackageResponse'

-- | /See:/ 'mkDeleteModelPackageResponse' smart constructor.
data DeleteModelPackageResponse = DeleteModelPackageResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteModelPackageResponse' value with any optional fields omitted.
mkDeleteModelPackageResponse ::
  DeleteModelPackageResponse
mkDeleteModelPackageResponse = DeleteModelPackageResponse'
