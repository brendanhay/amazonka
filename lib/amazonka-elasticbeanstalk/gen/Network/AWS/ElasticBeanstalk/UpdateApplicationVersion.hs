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
    uavVersionLabel,
    uavApplicationName,
    uavDescription,

    -- * Destructuring the response
    ApplicationVersionDescriptionMessage (..),
    mkApplicationVersionDescriptionMessage,

    -- ** Response lenses
    avdmApplicationVersion,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkUpdateApplicationVersion' smart constructor.
data UpdateApplicationVersion = UpdateApplicationVersion'
  { -- | The name of the version to update.
    --
    -- If no application version is found with this label, @UpdateApplication@ returns an @InvalidParameterValue@ error.
    versionLabel :: Lude.Text,
    -- | The name of the application associated with this version.
    --
    -- If no application is found with this name, @UpdateApplication@ returns an @InvalidParameterValue@ error.
    applicationName :: Lude.Text,
    -- | A new description for this version.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateApplicationVersion' with the minimum fields required to make a request.
--
-- * 'versionLabel' - The name of the version to update.
--
-- If no application version is found with this label, @UpdateApplication@ returns an @InvalidParameterValue@ error.
-- * 'applicationName' - The name of the application associated with this version.
--
-- If no application is found with this name, @UpdateApplication@ returns an @InvalidParameterValue@ error.
-- * 'description' - A new description for this version.
mkUpdateApplicationVersion ::
  -- | 'versionLabel'
  Lude.Text ->
  -- | 'applicationName'
  Lude.Text ->
  UpdateApplicationVersion
mkUpdateApplicationVersion pVersionLabel_ pApplicationName_ =
  UpdateApplicationVersion'
    { versionLabel = pVersionLabel_,
      applicationName = pApplicationName_,
      description = Lude.Nothing
    }

-- | The name of the version to update.
--
-- If no application version is found with this label, @UpdateApplication@ returns an @InvalidParameterValue@ error.
--
-- /Note:/ Consider using 'versionLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uavVersionLabel :: Lens.Lens' UpdateApplicationVersion Lude.Text
uavVersionLabel = Lens.lens (versionLabel :: UpdateApplicationVersion -> Lude.Text) (\s a -> s {versionLabel = a} :: UpdateApplicationVersion)
{-# DEPRECATED uavVersionLabel "Use generic-lens or generic-optics with 'versionLabel' instead." #-}

-- | The name of the application associated with this version.
--
-- If no application is found with this name, @UpdateApplication@ returns an @InvalidParameterValue@ error.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uavApplicationName :: Lens.Lens' UpdateApplicationVersion Lude.Text
uavApplicationName = Lens.lens (applicationName :: UpdateApplicationVersion -> Lude.Text) (\s a -> s {applicationName = a} :: UpdateApplicationVersion)
{-# DEPRECATED uavApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | A new description for this version.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uavDescription :: Lens.Lens' UpdateApplicationVersion (Lude.Maybe Lude.Text)
uavDescription = Lens.lens (description :: UpdateApplicationVersion -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateApplicationVersion)
{-# DEPRECATED uavDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest UpdateApplicationVersion where
  type
    Rs UpdateApplicationVersion =
      ApplicationVersionDescriptionMessage
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveXMLWrapper
      "UpdateApplicationVersionResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders UpdateApplicationVersion where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateApplicationVersion where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateApplicationVersion where
  toQuery UpdateApplicationVersion' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("UpdateApplicationVersion" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "VersionLabel" Lude.=: versionLabel,
        "ApplicationName" Lude.=: applicationName,
        "Description" Lude.=: description
      ]
