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
-- Module      : Amazonka.IoTThingsGraph.GetSystemTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a system.
module Amazonka.IoTThingsGraph.GetSystemTemplate
  ( -- * Creating a Request
    GetSystemTemplate (..),
    newGetSystemTemplate,

    -- * Request Lenses
    getSystemTemplate_revisionNumber,
    getSystemTemplate_id,

    -- * Destructuring the Response
    GetSystemTemplateResponse (..),
    newGetSystemTemplateResponse,

    -- * Response Lenses
    getSystemTemplateResponse_description,
    getSystemTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSystemTemplate' smart constructor.
data GetSystemTemplate = GetSystemTemplate'
  { -- | The number that specifies the revision of the system to get.
    revisionNumber :: Prelude.Maybe Prelude.Integer,
    -- | The ID of the system to get. This ID must be in the user\'s namespace.
    --
    -- The ID should be in the following format.
    --
    -- @urn:tdm:REGION\/ACCOUNT ID\/default:system:SYSTEMNAME@
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSystemTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revisionNumber', 'getSystemTemplate_revisionNumber' - The number that specifies the revision of the system to get.
--
-- 'id', 'getSystemTemplate_id' - The ID of the system to get. This ID must be in the user\'s namespace.
--
-- The ID should be in the following format.
--
-- @urn:tdm:REGION\/ACCOUNT ID\/default:system:SYSTEMNAME@
newGetSystemTemplate ::
  -- | 'id'
  Prelude.Text ->
  GetSystemTemplate
newGetSystemTemplate pId_ =
  GetSystemTemplate'
    { revisionNumber =
        Prelude.Nothing,
      id = pId_
    }

-- | The number that specifies the revision of the system to get.
getSystemTemplate_revisionNumber :: Lens.Lens' GetSystemTemplate (Prelude.Maybe Prelude.Integer)
getSystemTemplate_revisionNumber = Lens.lens (\GetSystemTemplate' {revisionNumber} -> revisionNumber) (\s@GetSystemTemplate' {} a -> s {revisionNumber = a} :: GetSystemTemplate)

-- | The ID of the system to get. This ID must be in the user\'s namespace.
--
-- The ID should be in the following format.
--
-- @urn:tdm:REGION\/ACCOUNT ID\/default:system:SYSTEMNAME@
getSystemTemplate_id :: Lens.Lens' GetSystemTemplate Prelude.Text
getSystemTemplate_id = Lens.lens (\GetSystemTemplate' {id} -> id) (\s@GetSystemTemplate' {} a -> s {id = a} :: GetSystemTemplate)

instance Core.AWSRequest GetSystemTemplate where
  type
    AWSResponse GetSystemTemplate =
      GetSystemTemplateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSystemTemplateResponse'
            Prelude.<$> (x Core..?> "description")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSystemTemplate where
  hashWithSalt _salt GetSystemTemplate' {..} =
    _salt `Prelude.hashWithSalt` revisionNumber
      `Prelude.hashWithSalt` id

instance Prelude.NFData GetSystemTemplate where
  rnf GetSystemTemplate' {..} =
    Prelude.rnf revisionNumber
      `Prelude.seq` Prelude.rnf id

instance Core.ToHeaders GetSystemTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IotThingsGraphFrontEndService.GetSystemTemplate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetSystemTemplate where
  toJSON GetSystemTemplate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("revisionNumber" Core..=)
              Prelude.<$> revisionNumber,
            Prelude.Just ("id" Core..= id)
          ]
      )

instance Core.ToPath GetSystemTemplate where
  toPath = Prelude.const "/"

instance Core.ToQuery GetSystemTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSystemTemplateResponse' smart constructor.
data GetSystemTemplateResponse = GetSystemTemplateResponse'
  { -- | An object that contains summary data about the system.
    description :: Prelude.Maybe SystemTemplateDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSystemTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'getSystemTemplateResponse_description' - An object that contains summary data about the system.
--
-- 'httpStatus', 'getSystemTemplateResponse_httpStatus' - The response's http status code.
newGetSystemTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSystemTemplateResponse
newGetSystemTemplateResponse pHttpStatus_ =
  GetSystemTemplateResponse'
    { description =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains summary data about the system.
getSystemTemplateResponse_description :: Lens.Lens' GetSystemTemplateResponse (Prelude.Maybe SystemTemplateDescription)
getSystemTemplateResponse_description = Lens.lens (\GetSystemTemplateResponse' {description} -> description) (\s@GetSystemTemplateResponse' {} a -> s {description = a} :: GetSystemTemplateResponse)

-- | The response's http status code.
getSystemTemplateResponse_httpStatus :: Lens.Lens' GetSystemTemplateResponse Prelude.Int
getSystemTemplateResponse_httpStatus = Lens.lens (\GetSystemTemplateResponse' {httpStatus} -> httpStatus) (\s@GetSystemTemplateResponse' {} a -> s {httpStatus = a} :: GetSystemTemplateResponse)

instance Prelude.NFData GetSystemTemplateResponse where
  rnf GetSystemTemplateResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf httpStatus
