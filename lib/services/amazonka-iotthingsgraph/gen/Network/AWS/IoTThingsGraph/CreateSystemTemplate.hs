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
-- Module      : Network.AWS.IoTThingsGraph.CreateSystemTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a system. The system is validated against the entities in the
-- latest version of the user\'s namespace unless another namespace version
-- is specified in the request.
module Network.AWS.IoTThingsGraph.CreateSystemTemplate
  ( -- * Creating a Request
    CreateSystemTemplate (..),
    newCreateSystemTemplate,

    -- * Request Lenses
    createSystemTemplate_compatibleNamespaceVersion,
    createSystemTemplate_definition,

    -- * Destructuring the Response
    CreateSystemTemplateResponse (..),
    newCreateSystemTemplateResponse,

    -- * Response Lenses
    createSystemTemplateResponse_summary,
    createSystemTemplateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTThingsGraph.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateSystemTemplate' smart constructor.
data CreateSystemTemplate = CreateSystemTemplate'
  { -- | The namespace version in which the system is to be created.
    --
    -- If no value is specified, the latest version is used by default.
    compatibleNamespaceVersion :: Prelude.Maybe Prelude.Integer,
    -- | The @DefinitionDocument@ used to create the system.
    definition :: DefinitionDocument
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSystemTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compatibleNamespaceVersion', 'createSystemTemplate_compatibleNamespaceVersion' - The namespace version in which the system is to be created.
--
-- If no value is specified, the latest version is used by default.
--
-- 'definition', 'createSystemTemplate_definition' - The @DefinitionDocument@ used to create the system.
newCreateSystemTemplate ::
  -- | 'definition'
  DefinitionDocument ->
  CreateSystemTemplate
newCreateSystemTemplate pDefinition_ =
  CreateSystemTemplate'
    { compatibleNamespaceVersion =
        Prelude.Nothing,
      definition = pDefinition_
    }

-- | The namespace version in which the system is to be created.
--
-- If no value is specified, the latest version is used by default.
createSystemTemplate_compatibleNamespaceVersion :: Lens.Lens' CreateSystemTemplate (Prelude.Maybe Prelude.Integer)
createSystemTemplate_compatibleNamespaceVersion = Lens.lens (\CreateSystemTemplate' {compatibleNamespaceVersion} -> compatibleNamespaceVersion) (\s@CreateSystemTemplate' {} a -> s {compatibleNamespaceVersion = a} :: CreateSystemTemplate)

-- | The @DefinitionDocument@ used to create the system.
createSystemTemplate_definition :: Lens.Lens' CreateSystemTemplate DefinitionDocument
createSystemTemplate_definition = Lens.lens (\CreateSystemTemplate' {definition} -> definition) (\s@CreateSystemTemplate' {} a -> s {definition = a} :: CreateSystemTemplate)

instance Core.AWSRequest CreateSystemTemplate where
  type
    AWSResponse CreateSystemTemplate =
      CreateSystemTemplateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSystemTemplateResponse'
            Prelude.<$> (x Core..?> "summary")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSystemTemplate

instance Prelude.NFData CreateSystemTemplate

instance Core.ToHeaders CreateSystemTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IotThingsGraphFrontEndService.CreateSystemTemplate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateSystemTemplate where
  toJSON CreateSystemTemplate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("compatibleNamespaceVersion" Core..=)
              Prelude.<$> compatibleNamespaceVersion,
            Prelude.Just ("definition" Core..= definition)
          ]
      )

instance Core.ToPath CreateSystemTemplate where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateSystemTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSystemTemplateResponse' smart constructor.
data CreateSystemTemplateResponse = CreateSystemTemplateResponse'
  { -- | The summary object that describes the created system.
    summary :: Prelude.Maybe SystemTemplateSummary,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSystemTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'summary', 'createSystemTemplateResponse_summary' - The summary object that describes the created system.
--
-- 'httpStatus', 'createSystemTemplateResponse_httpStatus' - The response's http status code.
newCreateSystemTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSystemTemplateResponse
newCreateSystemTemplateResponse pHttpStatus_ =
  CreateSystemTemplateResponse'
    { summary =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The summary object that describes the created system.
createSystemTemplateResponse_summary :: Lens.Lens' CreateSystemTemplateResponse (Prelude.Maybe SystemTemplateSummary)
createSystemTemplateResponse_summary = Lens.lens (\CreateSystemTemplateResponse' {summary} -> summary) (\s@CreateSystemTemplateResponse' {} a -> s {summary = a} :: CreateSystemTemplateResponse)

-- | The response's http status code.
createSystemTemplateResponse_httpStatus :: Lens.Lens' CreateSystemTemplateResponse Prelude.Int
createSystemTemplateResponse_httpStatus = Lens.lens (\CreateSystemTemplateResponse' {httpStatus} -> httpStatus) (\s@CreateSystemTemplateResponse' {} a -> s {httpStatus = a} :: CreateSystemTemplateResponse)

instance Prelude.NFData CreateSystemTemplateResponse
