{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DeleteConfigurationTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified configuration template.
--
-- When you launch an environment using a configuration template, the
-- environment gets a copy of the template. You can delete or modify the
-- environment\'s copy of the template without affecting the running
-- environment.
module Network.AWS.ElasticBeanstalk.DeleteConfigurationTemplate
  ( -- * Creating a Request
    DeleteConfigurationTemplate (..),
    newDeleteConfigurationTemplate,

    -- * Request Lenses
    deleteConfigurationTemplate_applicationName,
    deleteConfigurationTemplate_templateName,

    -- * Destructuring the Response
    DeleteConfigurationTemplateResponse (..),
    newDeleteConfigurationTemplateResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to delete a configuration template.
--
-- /See:/ 'newDeleteConfigurationTemplate' smart constructor.
data DeleteConfigurationTemplate = DeleteConfigurationTemplate'
  { -- | The name of the application to delete the configuration template from.
    applicationName :: Core.Text,
    -- | The name of the configuration template to delete.
    templateName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteConfigurationTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'deleteConfigurationTemplate_applicationName' - The name of the application to delete the configuration template from.
--
-- 'templateName', 'deleteConfigurationTemplate_templateName' - The name of the configuration template to delete.
newDeleteConfigurationTemplate ::
  -- | 'applicationName'
  Core.Text ->
  -- | 'templateName'
  Core.Text ->
  DeleteConfigurationTemplate
newDeleteConfigurationTemplate
  pApplicationName_
  pTemplateName_ =
    DeleteConfigurationTemplate'
      { applicationName =
          pApplicationName_,
        templateName = pTemplateName_
      }

-- | The name of the application to delete the configuration template from.
deleteConfigurationTemplate_applicationName :: Lens.Lens' DeleteConfigurationTemplate Core.Text
deleteConfigurationTemplate_applicationName = Lens.lens (\DeleteConfigurationTemplate' {applicationName} -> applicationName) (\s@DeleteConfigurationTemplate' {} a -> s {applicationName = a} :: DeleteConfigurationTemplate)

-- | The name of the configuration template to delete.
deleteConfigurationTemplate_templateName :: Lens.Lens' DeleteConfigurationTemplate Core.Text
deleteConfigurationTemplate_templateName = Lens.lens (\DeleteConfigurationTemplate' {templateName} -> templateName) (\s@DeleteConfigurationTemplate' {} a -> s {templateName = a} :: DeleteConfigurationTemplate)

instance Core.AWSRequest DeleteConfigurationTemplate where
  type
    AWSResponse DeleteConfigurationTemplate =
      DeleteConfigurationTemplateResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeleteConfigurationTemplateResponse'

instance Core.Hashable DeleteConfigurationTemplate

instance Core.NFData DeleteConfigurationTemplate

instance Core.ToHeaders DeleteConfigurationTemplate where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteConfigurationTemplate where
  toPath = Core.const "/"

instance Core.ToQuery DeleteConfigurationTemplate where
  toQuery DeleteConfigurationTemplate' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteConfigurationTemplate" :: Core.ByteString),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "ApplicationName" Core.=: applicationName,
        "TemplateName" Core.=: templateName
      ]

-- | /See:/ 'newDeleteConfigurationTemplateResponse' smart constructor.
data DeleteConfigurationTemplateResponse = DeleteConfigurationTemplateResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteConfigurationTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteConfigurationTemplateResponse ::
  DeleteConfigurationTemplateResponse
newDeleteConfigurationTemplateResponse =
  DeleteConfigurationTemplateResponse'

instance
  Core.NFData
    DeleteConfigurationTemplateResponse
