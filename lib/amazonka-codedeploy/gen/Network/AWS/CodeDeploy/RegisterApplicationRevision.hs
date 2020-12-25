{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.RegisterApplicationRevision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers with AWS CodeDeploy a revision for the specified application.
module Network.AWS.CodeDeploy.RegisterApplicationRevision
  ( -- * Creating a request
    RegisterApplicationRevision (..),
    mkRegisterApplicationRevision,

    -- ** Request lenses
    rarApplicationName,
    rarRevision,
    rarDescription,

    -- * Destructuring the response
    RegisterApplicationRevisionResponse (..),
    mkRegisterApplicationRevisionResponse,
  )
where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a RegisterApplicationRevision operation.
--
-- /See:/ 'mkRegisterApplicationRevision' smart constructor.
data RegisterApplicationRevision = RegisterApplicationRevision'
  { -- | The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
    applicationName :: Types.ApplicationName,
    -- | Information about the application revision to register, including type and location.
    revision :: Types.RevisionLocation,
    -- | A comment about the revision.
    description :: Core.Maybe Types.Description
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterApplicationRevision' value with any optional fields omitted.
mkRegisterApplicationRevision ::
  -- | 'applicationName'
  Types.ApplicationName ->
  -- | 'revision'
  Types.RevisionLocation ->
  RegisterApplicationRevision
mkRegisterApplicationRevision applicationName revision =
  RegisterApplicationRevision'
    { applicationName,
      revision,
      description = Core.Nothing
    }

-- | The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rarApplicationName :: Lens.Lens' RegisterApplicationRevision Types.ApplicationName
rarApplicationName = Lens.field @"applicationName"
{-# DEPRECATED rarApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | Information about the application revision to register, including type and location.
--
-- /Note:/ Consider using 'revision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rarRevision :: Lens.Lens' RegisterApplicationRevision Types.RevisionLocation
rarRevision = Lens.field @"revision"
{-# DEPRECATED rarRevision "Use generic-lens or generic-optics with 'revision' instead." #-}

-- | A comment about the revision.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rarDescription :: Lens.Lens' RegisterApplicationRevision (Core.Maybe Types.Description)
rarDescription = Lens.field @"description"
{-# DEPRECATED rarDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Core.FromJSON RegisterApplicationRevision where
  toJSON RegisterApplicationRevision {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("applicationName" Core..= applicationName),
            Core.Just ("revision" Core..= revision),
            ("description" Core..=) Core.<$> description
          ]
      )

instance Core.AWSRequest RegisterApplicationRevision where
  type
    Rs RegisterApplicationRevision =
      RegisterApplicationRevisionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeDeploy_20141006.RegisterApplicationRevision")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveNull RegisterApplicationRevisionResponse'

-- | /See:/ 'mkRegisterApplicationRevisionResponse' smart constructor.
data RegisterApplicationRevisionResponse = RegisterApplicationRevisionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterApplicationRevisionResponse' value with any optional fields omitted.
mkRegisterApplicationRevisionResponse ::
  RegisterApplicationRevisionResponse
mkRegisterApplicationRevisionResponse =
  RegisterApplicationRevisionResponse'
