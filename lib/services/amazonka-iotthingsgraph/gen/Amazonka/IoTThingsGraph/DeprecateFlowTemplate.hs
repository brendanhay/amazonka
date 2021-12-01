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
-- Module      : Amazonka.IoTThingsGraph.DeprecateFlowTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprecates the specified workflow. This action marks the workflow for
-- deletion. Deprecated flows can\'t be deployed, but existing deployments
-- will continue to run.
module Amazonka.IoTThingsGraph.DeprecateFlowTemplate
  ( -- * Creating a Request
    DeprecateFlowTemplate (..),
    newDeprecateFlowTemplate,

    -- * Request Lenses
    deprecateFlowTemplate_id,

    -- * Destructuring the Response
    DeprecateFlowTemplateResponse (..),
    newDeprecateFlowTemplateResponse,

    -- * Response Lenses
    deprecateFlowTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeprecateFlowTemplate' smart constructor.
data DeprecateFlowTemplate = DeprecateFlowTemplate'
  { -- | The ID of the workflow to be deleted.
    --
    -- The ID should be in the following format.
    --
    -- @urn:tdm:REGION\/ACCOUNT ID\/default:workflow:WORKFLOWNAME@
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeprecateFlowTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deprecateFlowTemplate_id' - The ID of the workflow to be deleted.
--
-- The ID should be in the following format.
--
-- @urn:tdm:REGION\/ACCOUNT ID\/default:workflow:WORKFLOWNAME@
newDeprecateFlowTemplate ::
  -- | 'id'
  Prelude.Text ->
  DeprecateFlowTemplate
newDeprecateFlowTemplate pId_ =
  DeprecateFlowTemplate' {id = pId_}

-- | The ID of the workflow to be deleted.
--
-- The ID should be in the following format.
--
-- @urn:tdm:REGION\/ACCOUNT ID\/default:workflow:WORKFLOWNAME@
deprecateFlowTemplate_id :: Lens.Lens' DeprecateFlowTemplate Prelude.Text
deprecateFlowTemplate_id = Lens.lens (\DeprecateFlowTemplate' {id} -> id) (\s@DeprecateFlowTemplate' {} a -> s {id = a} :: DeprecateFlowTemplate)

instance Core.AWSRequest DeprecateFlowTemplate where
  type
    AWSResponse DeprecateFlowTemplate =
      DeprecateFlowTemplateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeprecateFlowTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeprecateFlowTemplate where
  hashWithSalt salt' DeprecateFlowTemplate' {..} =
    salt' `Prelude.hashWithSalt` id

instance Prelude.NFData DeprecateFlowTemplate where
  rnf DeprecateFlowTemplate' {..} = Prelude.rnf id

instance Core.ToHeaders DeprecateFlowTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IotThingsGraphFrontEndService.DeprecateFlowTemplate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeprecateFlowTemplate where
  toJSON DeprecateFlowTemplate' {..} =
    Core.object
      (Prelude.catMaybes [Prelude.Just ("id" Core..= id)])

instance Core.ToPath DeprecateFlowTemplate where
  toPath = Prelude.const "/"

instance Core.ToQuery DeprecateFlowTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeprecateFlowTemplateResponse' smart constructor.
data DeprecateFlowTemplateResponse = DeprecateFlowTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeprecateFlowTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deprecateFlowTemplateResponse_httpStatus' - The response's http status code.
newDeprecateFlowTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeprecateFlowTemplateResponse
newDeprecateFlowTemplateResponse pHttpStatus_ =
  DeprecateFlowTemplateResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deprecateFlowTemplateResponse_httpStatus :: Lens.Lens' DeprecateFlowTemplateResponse Prelude.Int
deprecateFlowTemplateResponse_httpStatus = Lens.lens (\DeprecateFlowTemplateResponse' {httpStatus} -> httpStatus) (\s@DeprecateFlowTemplateResponse' {} a -> s {httpStatus = a} :: DeprecateFlowTemplateResponse)

instance Prelude.NFData DeprecateFlowTemplateResponse where
  rnf DeprecateFlowTemplateResponse' {..} =
    Prelude.rnf httpStatus
