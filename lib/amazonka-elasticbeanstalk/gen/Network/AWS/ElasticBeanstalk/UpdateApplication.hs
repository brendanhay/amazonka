{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.UpdateApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified application to have the specified properties.
module Network.AWS.ElasticBeanstalk.UpdateApplication
  ( -- * Creating a request
    UpdateApplication (..),
    mkUpdateApplication,

    -- ** Request lenses
    uaApplicationName,
    uaDescription,

    -- * Destructuring the response
    ApplicationDescriptionMessage (..),
    mkApplicationDescriptionMessage,

    -- ** Response lenses
    admApplication,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to update an application.
--
-- /See:/ 'mkUpdateApplication' smart constructor.
data UpdateApplication = UpdateApplication'
  { -- | The name of the application to update. If no such application is found, @UpdateApplication@ returns an @InvalidParameterValue@ error.
    applicationName :: Lude.Text,
    -- | A new description for the application.
    --
    -- Default: If not specified, AWS Elastic Beanstalk does not update the description.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateApplication' with the minimum fields required to make a request.
--
-- * 'applicationName' - The name of the application to update. If no such application is found, @UpdateApplication@ returns an @InvalidParameterValue@ error.
-- * 'description' - A new description for the application.
--
-- Default: If not specified, AWS Elastic Beanstalk does not update the description.
mkUpdateApplication ::
  -- | 'applicationName'
  Lude.Text ->
  UpdateApplication
mkUpdateApplication pApplicationName_ =
  UpdateApplication'
    { applicationName = pApplicationName_,
      description = Lude.Nothing
    }

-- | The name of the application to update. If no such application is found, @UpdateApplication@ returns an @InvalidParameterValue@ error.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaApplicationName :: Lens.Lens' UpdateApplication Lude.Text
uaApplicationName = Lens.lens (applicationName :: UpdateApplication -> Lude.Text) (\s a -> s {applicationName = a} :: UpdateApplication)
{-# DEPRECATED uaApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | A new description for the application.
--
-- Default: If not specified, AWS Elastic Beanstalk does not update the description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaDescription :: Lens.Lens' UpdateApplication (Lude.Maybe Lude.Text)
uaDescription = Lens.lens (description :: UpdateApplication -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateApplication)
{-# DEPRECATED uaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest UpdateApplication where
  type Rs UpdateApplication = ApplicationDescriptionMessage
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveXMLWrapper
      "UpdateApplicationResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders UpdateApplication where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateApplication where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateApplication where
  toQuery UpdateApplication' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("UpdateApplication" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "ApplicationName" Lude.=: applicationName,
        "Description" Lude.=: description
      ]
