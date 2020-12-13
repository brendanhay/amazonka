{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DeleteConfigurationTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified configuration template.
module Network.AWS.ElasticBeanstalk.DeleteConfigurationTemplate
  ( -- * Creating a request
    DeleteConfigurationTemplate (..),
    mkDeleteConfigurationTemplate,

    -- ** Request lenses
    dctTemplateName,
    dctApplicationName,

    -- * Destructuring the response
    DeleteConfigurationTemplateResponse (..),
    mkDeleteConfigurationTemplateResponse,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to delete a configuration template.
--
-- /See:/ 'mkDeleteConfigurationTemplate' smart constructor.
data DeleteConfigurationTemplate = DeleteConfigurationTemplate'
  { -- | The name of the configuration template to delete.
    templateName :: Lude.Text,
    -- | The name of the application to delete the configuration template from.
    applicationName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteConfigurationTemplate' with the minimum fields required to make a request.
--
-- * 'templateName' - The name of the configuration template to delete.
-- * 'applicationName' - The name of the application to delete the configuration template from.
mkDeleteConfigurationTemplate ::
  -- | 'templateName'
  Lude.Text ->
  -- | 'applicationName'
  Lude.Text ->
  DeleteConfigurationTemplate
mkDeleteConfigurationTemplate pTemplateName_ pApplicationName_ =
  DeleteConfigurationTemplate'
    { templateName = pTemplateName_,
      applicationName = pApplicationName_
    }

-- | The name of the configuration template to delete.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dctTemplateName :: Lens.Lens' DeleteConfigurationTemplate Lude.Text
dctTemplateName = Lens.lens (templateName :: DeleteConfigurationTemplate -> Lude.Text) (\s a -> s {templateName = a} :: DeleteConfigurationTemplate)
{-# DEPRECATED dctTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The name of the application to delete the configuration template from.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dctApplicationName :: Lens.Lens' DeleteConfigurationTemplate Lude.Text
dctApplicationName = Lens.lens (applicationName :: DeleteConfigurationTemplate -> Lude.Text) (\s a -> s {applicationName = a} :: DeleteConfigurationTemplate)
{-# DEPRECATED dctApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

instance Lude.AWSRequest DeleteConfigurationTemplate where
  type
    Rs DeleteConfigurationTemplate =
      DeleteConfigurationTemplateResponse
  request = Req.postQuery elasticBeanstalkService
  response = Res.receiveNull DeleteConfigurationTemplateResponse'

instance Lude.ToHeaders DeleteConfigurationTemplate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteConfigurationTemplate where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteConfigurationTemplate where
  toQuery DeleteConfigurationTemplate' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteConfigurationTemplate" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "TemplateName" Lude.=: templateName,
        "ApplicationName" Lude.=: applicationName
      ]

-- | /See:/ 'mkDeleteConfigurationTemplateResponse' smart constructor.
data DeleteConfigurationTemplateResponse = DeleteConfigurationTemplateResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteConfigurationTemplateResponse' with the minimum fields required to make a request.
mkDeleteConfigurationTemplateResponse ::
  DeleteConfigurationTemplateResponse
mkDeleteConfigurationTemplateResponse =
  DeleteConfigurationTemplateResponse'
