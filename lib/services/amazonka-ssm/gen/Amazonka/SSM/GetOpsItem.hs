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
-- Module      : Amazonka.SSM.GetOpsItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get information about an OpsItem by using the ID. You must have
-- permission in Identity and Access Management (IAM) to view information
-- about an OpsItem. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-getting-started.html Getting started with OpsCenter>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- Operations engineers and IT professionals use Amazon Web Services
-- Systems Manager OpsCenter to view, investigate, and remediate
-- operational issues impacting the performance and health of their Amazon
-- Web Services resources. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter.html OpsCenter>
-- in the /Amazon Web Services Systems Manager User Guide/.
module Amazonka.SSM.GetOpsItem
  ( -- * Creating a Request
    GetOpsItem (..),
    newGetOpsItem,

    -- * Request Lenses
    getOpsItem_opsItemArn,
    getOpsItem_opsItemId,

    -- * Destructuring the Response
    GetOpsItemResponse (..),
    newGetOpsItemResponse,

    -- * Response Lenses
    getOpsItemResponse_opsItem,
    getOpsItemResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newGetOpsItem' smart constructor.
data GetOpsItem = GetOpsItem'
  { -- | The OpsItem Amazon Resource Name (ARN).
    opsItemArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the OpsItem that you want to get.
    opsItemId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOpsItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'opsItemArn', 'getOpsItem_opsItemArn' - The OpsItem Amazon Resource Name (ARN).
--
-- 'opsItemId', 'getOpsItem_opsItemId' - The ID of the OpsItem that you want to get.
newGetOpsItem ::
  -- | 'opsItemId'
  Prelude.Text ->
  GetOpsItem
newGetOpsItem pOpsItemId_ =
  GetOpsItem'
    { opsItemArn = Prelude.Nothing,
      opsItemId = pOpsItemId_
    }

-- | The OpsItem Amazon Resource Name (ARN).
getOpsItem_opsItemArn :: Lens.Lens' GetOpsItem (Prelude.Maybe Prelude.Text)
getOpsItem_opsItemArn = Lens.lens (\GetOpsItem' {opsItemArn} -> opsItemArn) (\s@GetOpsItem' {} a -> s {opsItemArn = a} :: GetOpsItem)

-- | The ID of the OpsItem that you want to get.
getOpsItem_opsItemId :: Lens.Lens' GetOpsItem Prelude.Text
getOpsItem_opsItemId = Lens.lens (\GetOpsItem' {opsItemId} -> opsItemId) (\s@GetOpsItem' {} a -> s {opsItemId = a} :: GetOpsItem)

instance Core.AWSRequest GetOpsItem where
  type AWSResponse GetOpsItem = GetOpsItemResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOpsItemResponse'
            Prelude.<$> (x Core..?> "OpsItem")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetOpsItem where
  hashWithSalt _salt GetOpsItem' {..} =
    _salt `Prelude.hashWithSalt` opsItemArn
      `Prelude.hashWithSalt` opsItemId

instance Prelude.NFData GetOpsItem where
  rnf GetOpsItem' {..} =
    Prelude.rnf opsItemArn
      `Prelude.seq` Prelude.rnf opsItemId

instance Core.ToHeaders GetOpsItem where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.GetOpsItem" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetOpsItem where
  toJSON GetOpsItem' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("OpsItemArn" Core..=) Prelude.<$> opsItemArn,
            Prelude.Just ("OpsItemId" Core..= opsItemId)
          ]
      )

instance Core.ToPath GetOpsItem where
  toPath = Prelude.const "/"

instance Core.ToQuery GetOpsItem where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetOpsItemResponse' smart constructor.
data GetOpsItemResponse = GetOpsItemResponse'
  { -- | The OpsItem.
    opsItem :: Prelude.Maybe OpsItem,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOpsItemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'opsItem', 'getOpsItemResponse_opsItem' - The OpsItem.
--
-- 'httpStatus', 'getOpsItemResponse_httpStatus' - The response's http status code.
newGetOpsItemResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetOpsItemResponse
newGetOpsItemResponse pHttpStatus_ =
  GetOpsItemResponse'
    { opsItem = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The OpsItem.
getOpsItemResponse_opsItem :: Lens.Lens' GetOpsItemResponse (Prelude.Maybe OpsItem)
getOpsItemResponse_opsItem = Lens.lens (\GetOpsItemResponse' {opsItem} -> opsItem) (\s@GetOpsItemResponse' {} a -> s {opsItem = a} :: GetOpsItemResponse)

-- | The response's http status code.
getOpsItemResponse_httpStatus :: Lens.Lens' GetOpsItemResponse Prelude.Int
getOpsItemResponse_httpStatus = Lens.lens (\GetOpsItemResponse' {httpStatus} -> httpStatus) (\s@GetOpsItemResponse' {} a -> s {httpStatus = a} :: GetOpsItemResponse)

instance Prelude.NFData GetOpsItemResponse where
  rnf GetOpsItemResponse' {..} =
    Prelude.rnf opsItem
      `Prelude.seq` Prelude.rnf httpStatus
