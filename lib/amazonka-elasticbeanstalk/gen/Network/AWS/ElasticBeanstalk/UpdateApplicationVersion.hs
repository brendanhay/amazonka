{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.UpdateApplicationVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified application version to have the specified properties.
module Network.AWS.ElasticBeanstalk.UpdateApplicationVersion
  ( -- * Creating a request
    UpdateApplicationVersion (..),
    mkUpdateApplicationVersion,

    -- ** Request lenses
    uavApplicationName,
    uavVersionLabel,
    uavDescription,

    -- * Destructuring the response
    Types.ApplicationVersionDescriptionMessage (..),
    Types.mkApplicationVersionDescriptionMessage,

    -- ** Response lenses
    Types.avdmApplicationVersion,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkUpdateApplicationVersion' smart constructor.
data UpdateApplicationVersion = UpdateApplicationVersion'
  { -- | The name of the application associated with this version.
    --
    -- If no application is found with this name, @UpdateApplication@ returns an @InvalidParameterValue@ error.
    applicationName :: Types.ApplicationName,
    -- | The name of the version to update.
    --
    -- If no application version is found with this label, @UpdateApplication@ returns an @InvalidParameterValue@ error.
    versionLabel :: Types.VersionLabel,
    -- | A new description for this version.
    description :: Core.Maybe Types.Description
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApplicationVersion' value with any optional fields omitted.
mkUpdateApplicationVersion ::
  -- | 'applicationName'
  Types.ApplicationName ->
  -- | 'versionLabel'
  Types.VersionLabel ->
  UpdateApplicationVersion
mkUpdateApplicationVersion applicationName versionLabel =
  UpdateApplicationVersion'
    { applicationName,
      versionLabel,
      description = Core.Nothing
    }

-- | The name of the application associated with this version.
--
-- If no application is found with this name, @UpdateApplication@ returns an @InvalidParameterValue@ error.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uavApplicationName :: Lens.Lens' UpdateApplicationVersion Types.ApplicationName
uavApplicationName = Lens.field @"applicationName"
{-# DEPRECATED uavApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The name of the version to update.
--
-- If no application version is found with this label, @UpdateApplication@ returns an @InvalidParameterValue@ error.
--
-- /Note:/ Consider using 'versionLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uavVersionLabel :: Lens.Lens' UpdateApplicationVersion Types.VersionLabel
uavVersionLabel = Lens.field @"versionLabel"
{-# DEPRECATED uavVersionLabel "Use generic-lens or generic-optics with 'versionLabel' instead." #-}

-- | A new description for this version.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uavDescription :: Lens.Lens' UpdateApplicationVersion (Core.Maybe Types.Description)
uavDescription = Lens.field @"description"
{-# DEPRECATED uavDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Core.AWSRequest UpdateApplicationVersion where
  type
    Rs UpdateApplicationVersion =
      Types.ApplicationVersionDescriptionMessage
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "UpdateApplicationVersion")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "ApplicationName" applicationName)
                Core.<> (Core.toQueryValue "VersionLabel" versionLabel)
                Core.<> (Core.toQueryValue "Description" Core.<$> description)
            )
      }
  response =
    Response.receiveXMLWrapper
      "UpdateApplicationVersionResult"
      (\s h x -> Core.parseXML x)
