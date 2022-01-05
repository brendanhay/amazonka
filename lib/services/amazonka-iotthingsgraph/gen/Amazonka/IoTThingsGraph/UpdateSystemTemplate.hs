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
-- Module      : Amazonka.IoTThingsGraph.UpdateSystemTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified system. You don\'t need to run this action after
-- updating a workflow. Any deployment that uses the system will see the
-- changes in the system when it is redeployed.
module Amazonka.IoTThingsGraph.UpdateSystemTemplate
  ( -- * Creating a Request
    UpdateSystemTemplate (..),
    newUpdateSystemTemplate,

    -- * Request Lenses
    updateSystemTemplate_compatibleNamespaceVersion,
    updateSystemTemplate_id,
    updateSystemTemplate_definition,

    -- * Destructuring the Response
    UpdateSystemTemplateResponse (..),
    newUpdateSystemTemplateResponse,

    -- * Response Lenses
    updateSystemTemplateResponse_summary,
    updateSystemTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSystemTemplate' smart constructor.
data UpdateSystemTemplate = UpdateSystemTemplate'
  { -- | The version of the user\'s namespace. Defaults to the latest version of
    -- the user\'s namespace.
    --
    -- If no value is specified, the latest version is used by default.
    compatibleNamespaceVersion :: Prelude.Maybe Prelude.Integer,
    -- | The ID of the system to be updated.
    --
    -- The ID should be in the following format.
    --
    -- @urn:tdm:REGION\/ACCOUNT ID\/default:system:SYSTEMNAME@
    id :: Prelude.Text,
    -- | The @DefinitionDocument@ that contains the updated system definition.
    definition :: DefinitionDocument
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSystemTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compatibleNamespaceVersion', 'updateSystemTemplate_compatibleNamespaceVersion' - The version of the user\'s namespace. Defaults to the latest version of
-- the user\'s namespace.
--
-- If no value is specified, the latest version is used by default.
--
-- 'id', 'updateSystemTemplate_id' - The ID of the system to be updated.
--
-- The ID should be in the following format.
--
-- @urn:tdm:REGION\/ACCOUNT ID\/default:system:SYSTEMNAME@
--
-- 'definition', 'updateSystemTemplate_definition' - The @DefinitionDocument@ that contains the updated system definition.
newUpdateSystemTemplate ::
  -- | 'id'
  Prelude.Text ->
  -- | 'definition'
  DefinitionDocument ->
  UpdateSystemTemplate
newUpdateSystemTemplate pId_ pDefinition_ =
  UpdateSystemTemplate'
    { compatibleNamespaceVersion =
        Prelude.Nothing,
      id = pId_,
      definition = pDefinition_
    }

-- | The version of the user\'s namespace. Defaults to the latest version of
-- the user\'s namespace.
--
-- If no value is specified, the latest version is used by default.
updateSystemTemplate_compatibleNamespaceVersion :: Lens.Lens' UpdateSystemTemplate (Prelude.Maybe Prelude.Integer)
updateSystemTemplate_compatibleNamespaceVersion = Lens.lens (\UpdateSystemTemplate' {compatibleNamespaceVersion} -> compatibleNamespaceVersion) (\s@UpdateSystemTemplate' {} a -> s {compatibleNamespaceVersion = a} :: UpdateSystemTemplate)

-- | The ID of the system to be updated.
--
-- The ID should be in the following format.
--
-- @urn:tdm:REGION\/ACCOUNT ID\/default:system:SYSTEMNAME@
updateSystemTemplate_id :: Lens.Lens' UpdateSystemTemplate Prelude.Text
updateSystemTemplate_id = Lens.lens (\UpdateSystemTemplate' {id} -> id) (\s@UpdateSystemTemplate' {} a -> s {id = a} :: UpdateSystemTemplate)

-- | The @DefinitionDocument@ that contains the updated system definition.
updateSystemTemplate_definition :: Lens.Lens' UpdateSystemTemplate DefinitionDocument
updateSystemTemplate_definition = Lens.lens (\UpdateSystemTemplate' {definition} -> definition) (\s@UpdateSystemTemplate' {} a -> s {definition = a} :: UpdateSystemTemplate)

instance Core.AWSRequest UpdateSystemTemplate where
  type
    AWSResponse UpdateSystemTemplate =
      UpdateSystemTemplateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSystemTemplateResponse'
            Prelude.<$> (x Core..?> "summary")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSystemTemplate where
  hashWithSalt _salt UpdateSystemTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` compatibleNamespaceVersion
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` definition

instance Prelude.NFData UpdateSystemTemplate where
  rnf UpdateSystemTemplate' {..} =
    Prelude.rnf compatibleNamespaceVersion
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf definition

instance Core.ToHeaders UpdateSystemTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IotThingsGraphFrontEndService.UpdateSystemTemplate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateSystemTemplate where
  toJSON UpdateSystemTemplate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("compatibleNamespaceVersion" Core..=)
              Prelude.<$> compatibleNamespaceVersion,
            Prelude.Just ("id" Core..= id),
            Prelude.Just ("definition" Core..= definition)
          ]
      )

instance Core.ToPath UpdateSystemTemplate where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateSystemTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSystemTemplateResponse' smart constructor.
data UpdateSystemTemplateResponse = UpdateSystemTemplateResponse'
  { -- | An object containing summary information about the updated system.
    summary :: Prelude.Maybe SystemTemplateSummary,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSystemTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'summary', 'updateSystemTemplateResponse_summary' - An object containing summary information about the updated system.
--
-- 'httpStatus', 'updateSystemTemplateResponse_httpStatus' - The response's http status code.
newUpdateSystemTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSystemTemplateResponse
newUpdateSystemTemplateResponse pHttpStatus_ =
  UpdateSystemTemplateResponse'
    { summary =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object containing summary information about the updated system.
updateSystemTemplateResponse_summary :: Lens.Lens' UpdateSystemTemplateResponse (Prelude.Maybe SystemTemplateSummary)
updateSystemTemplateResponse_summary = Lens.lens (\UpdateSystemTemplateResponse' {summary} -> summary) (\s@UpdateSystemTemplateResponse' {} a -> s {summary = a} :: UpdateSystemTemplateResponse)

-- | The response's http status code.
updateSystemTemplateResponse_httpStatus :: Lens.Lens' UpdateSystemTemplateResponse Prelude.Int
updateSystemTemplateResponse_httpStatus = Lens.lens (\UpdateSystemTemplateResponse' {httpStatus} -> httpStatus) (\s@UpdateSystemTemplateResponse' {} a -> s {httpStatus = a} :: UpdateSystemTemplateResponse)

instance Prelude.NFData UpdateSystemTemplateResponse where
  rnf UpdateSystemTemplateResponse' {..} =
    Prelude.rnf summary
      `Prelude.seq` Prelude.rnf httpStatus
