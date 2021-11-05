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
-- Module      : Amazonka.IoTThingsGraph.DeprecateSystemTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprecates the specified system.
module Amazonka.IoTThingsGraph.DeprecateSystemTemplate
  ( -- * Creating a Request
    DeprecateSystemTemplate (..),
    newDeprecateSystemTemplate,

    -- * Request Lenses
    deprecateSystemTemplate_id,

    -- * Destructuring the Response
    DeprecateSystemTemplateResponse (..),
    newDeprecateSystemTemplateResponse,

    -- * Response Lenses
    deprecateSystemTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeprecateSystemTemplate' smart constructor.
data DeprecateSystemTemplate = DeprecateSystemTemplate'
  { -- | The ID of the system to delete.
    --
    -- The ID should be in the following format.
    --
    -- @urn:tdm:REGION\/ACCOUNT ID\/default:system:SYSTEMNAME@
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeprecateSystemTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deprecateSystemTemplate_id' - The ID of the system to delete.
--
-- The ID should be in the following format.
--
-- @urn:tdm:REGION\/ACCOUNT ID\/default:system:SYSTEMNAME@
newDeprecateSystemTemplate ::
  -- | 'id'
  Prelude.Text ->
  DeprecateSystemTemplate
newDeprecateSystemTemplate pId_ =
  DeprecateSystemTemplate' {id = pId_}

-- | The ID of the system to delete.
--
-- The ID should be in the following format.
--
-- @urn:tdm:REGION\/ACCOUNT ID\/default:system:SYSTEMNAME@
deprecateSystemTemplate_id :: Lens.Lens' DeprecateSystemTemplate Prelude.Text
deprecateSystemTemplate_id = Lens.lens (\DeprecateSystemTemplate' {id} -> id) (\s@DeprecateSystemTemplate' {} a -> s {id = a} :: DeprecateSystemTemplate)

instance Core.AWSRequest DeprecateSystemTemplate where
  type
    AWSResponse DeprecateSystemTemplate =
      DeprecateSystemTemplateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeprecateSystemTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeprecateSystemTemplate

instance Prelude.NFData DeprecateSystemTemplate

instance Core.ToHeaders DeprecateSystemTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IotThingsGraphFrontEndService.DeprecateSystemTemplate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeprecateSystemTemplate where
  toJSON DeprecateSystemTemplate' {..} =
    Core.object
      (Prelude.catMaybes [Prelude.Just ("id" Core..= id)])

instance Core.ToPath DeprecateSystemTemplate where
  toPath = Prelude.const "/"

instance Core.ToQuery DeprecateSystemTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeprecateSystemTemplateResponse' smart constructor.
data DeprecateSystemTemplateResponse = DeprecateSystemTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeprecateSystemTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deprecateSystemTemplateResponse_httpStatus' - The response's http status code.
newDeprecateSystemTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeprecateSystemTemplateResponse
newDeprecateSystemTemplateResponse pHttpStatus_ =
  DeprecateSystemTemplateResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deprecateSystemTemplateResponse_httpStatus :: Lens.Lens' DeprecateSystemTemplateResponse Prelude.Int
deprecateSystemTemplateResponse_httpStatus = Lens.lens (\DeprecateSystemTemplateResponse' {httpStatus} -> httpStatus) (\s@DeprecateSystemTemplateResponse' {} a -> s {httpStatus = a} :: DeprecateSystemTemplateResponse)

instance
  Prelude.NFData
    DeprecateSystemTemplateResponse
