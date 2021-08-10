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
-- Module      : Network.AWS.SSM.GetOpsItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get information about an OpsItem by using the ID. You must have
-- permission in AWS Identity and Access Management (IAM) to view
-- information about an OpsItem. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-getting-started.html Getting started with OpsCenter>
-- in the /AWS Systems Manager User Guide/.
--
-- Operations engineers and IT professionals use OpsCenter to view,
-- investigate, and remediate operational issues impacting the performance
-- and health of their AWS resources. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter.html AWS Systems Manager OpsCenter>
-- in the /AWS Systems Manager User Guide/.
module Network.AWS.SSM.GetOpsItem
  ( -- * Creating a Request
    GetOpsItem (..),
    newGetOpsItem,

    -- * Request Lenses
    getOpsItem_opsItemId,

    -- * Destructuring the Response
    GetOpsItemResponse (..),
    newGetOpsItemResponse,

    -- * Response Lenses
    getOpsItemResponse_opsItem,
    getOpsItemResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newGetOpsItem' smart constructor.
data GetOpsItem = GetOpsItem'
  { -- | The ID of the OpsItem that you want to get.
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
-- 'opsItemId', 'getOpsItem_opsItemId' - The ID of the OpsItem that you want to get.
newGetOpsItem ::
  -- | 'opsItemId'
  Prelude.Text ->
  GetOpsItem
newGetOpsItem pOpsItemId_ =
  GetOpsItem' {opsItemId = pOpsItemId_}

-- | The ID of the OpsItem that you want to get.
getOpsItem_opsItemId :: Lens.Lens' GetOpsItem Prelude.Text
getOpsItem_opsItemId = Lens.lens (\GetOpsItem' {opsItemId} -> opsItemId) (\s@GetOpsItem' {} a -> s {opsItemId = a} :: GetOpsItem)

instance Core.AWSRequest GetOpsItem where
  type AWSResponse GetOpsItem = GetOpsItemResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOpsItemResponse'
            Prelude.<$> (x Core..?> "OpsItem")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetOpsItem

instance Prelude.NFData GetOpsItem

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
          [Prelude.Just ("OpsItemId" Core..= opsItemId)]
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

instance Prelude.NFData GetOpsItemResponse
