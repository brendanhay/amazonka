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
-- Module      : Amazonka.ElasticBeanstalk.DeleteConfigurationTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.ElasticBeanstalk.DeleteConfigurationTemplate
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to delete a configuration template.
--
-- /See:/ 'newDeleteConfigurationTemplate' smart constructor.
data DeleteConfigurationTemplate = DeleteConfigurationTemplate'
  { -- | The name of the application to delete the configuration template from.
    applicationName :: Prelude.Text,
    -- | The name of the configuration template to delete.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'templateName'
  Prelude.Text ->
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
deleteConfigurationTemplate_applicationName :: Lens.Lens' DeleteConfigurationTemplate Prelude.Text
deleteConfigurationTemplate_applicationName = Lens.lens (\DeleteConfigurationTemplate' {applicationName} -> applicationName) (\s@DeleteConfigurationTemplate' {} a -> s {applicationName = a} :: DeleteConfigurationTemplate)

-- | The name of the configuration template to delete.
deleteConfigurationTemplate_templateName :: Lens.Lens' DeleteConfigurationTemplate Prelude.Text
deleteConfigurationTemplate_templateName = Lens.lens (\DeleteConfigurationTemplate' {templateName} -> templateName) (\s@DeleteConfigurationTemplate' {} a -> s {templateName = a} :: DeleteConfigurationTemplate)

instance Core.AWSRequest DeleteConfigurationTemplate where
  type
    AWSResponse DeleteConfigurationTemplate =
      DeleteConfigurationTemplateResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      DeleteConfigurationTemplateResponse'

instance Prelude.Hashable DeleteConfigurationTemplate where
  hashWithSalt _salt DeleteConfigurationTemplate' {..} =
    _salt `Prelude.hashWithSalt` applicationName
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData DeleteConfigurationTemplate where
  rnf DeleteConfigurationTemplate' {..} =
    Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf templateName

instance Data.ToHeaders DeleteConfigurationTemplate where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteConfigurationTemplate where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteConfigurationTemplate where
  toQuery DeleteConfigurationTemplate' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DeleteConfigurationTemplate" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "ApplicationName" Data.=: applicationName,
        "TemplateName" Data.=: templateName
      ]

-- | /See:/ 'newDeleteConfigurationTemplateResponse' smart constructor.
data DeleteConfigurationTemplateResponse = DeleteConfigurationTemplateResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConfigurationTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteConfigurationTemplateResponse ::
  DeleteConfigurationTemplateResponse
newDeleteConfigurationTemplateResponse =
  DeleteConfigurationTemplateResponse'

instance
  Prelude.NFData
    DeleteConfigurationTemplateResponse
  where
  rnf _ = ()
