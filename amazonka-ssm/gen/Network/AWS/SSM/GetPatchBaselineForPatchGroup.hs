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
-- Module      : Network.AWS.SSM.GetPatchBaselineForPatchGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the patch baseline that should be used for the specified patch
-- group.
module Network.AWS.SSM.GetPatchBaselineForPatchGroup
  ( -- * Creating a Request
    GetPatchBaselineForPatchGroup (..),
    newGetPatchBaselineForPatchGroup,

    -- * Request Lenses
    getPatchBaselineForPatchGroup_operatingSystem,
    getPatchBaselineForPatchGroup_patchGroup,

    -- * Destructuring the Response
    GetPatchBaselineForPatchGroupResponse (..),
    newGetPatchBaselineForPatchGroupResponse,

    -- * Response Lenses
    getPatchBaselineForPatchGroupResponse_baselineId,
    getPatchBaselineForPatchGroupResponse_patchGroup,
    getPatchBaselineForPatchGroupResponse_operatingSystem,
    getPatchBaselineForPatchGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newGetPatchBaselineForPatchGroup' smart constructor.
data GetPatchBaselineForPatchGroup = GetPatchBaselineForPatchGroup'
  { -- | Returns he operating system rule specified for patch groups using the
    -- patch baseline.
    operatingSystem :: Prelude.Maybe OperatingSystem,
    -- | The name of the patch group whose patch baseline should be retrieved.
    patchGroup :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPatchBaselineForPatchGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operatingSystem', 'getPatchBaselineForPatchGroup_operatingSystem' - Returns he operating system rule specified for patch groups using the
-- patch baseline.
--
-- 'patchGroup', 'getPatchBaselineForPatchGroup_patchGroup' - The name of the patch group whose patch baseline should be retrieved.
newGetPatchBaselineForPatchGroup ::
  -- | 'patchGroup'
  Prelude.Text ->
  GetPatchBaselineForPatchGroup
newGetPatchBaselineForPatchGroup pPatchGroup_ =
  GetPatchBaselineForPatchGroup'
    { operatingSystem =
        Prelude.Nothing,
      patchGroup = pPatchGroup_
    }

-- | Returns he operating system rule specified for patch groups using the
-- patch baseline.
getPatchBaselineForPatchGroup_operatingSystem :: Lens.Lens' GetPatchBaselineForPatchGroup (Prelude.Maybe OperatingSystem)
getPatchBaselineForPatchGroup_operatingSystem = Lens.lens (\GetPatchBaselineForPatchGroup' {operatingSystem} -> operatingSystem) (\s@GetPatchBaselineForPatchGroup' {} a -> s {operatingSystem = a} :: GetPatchBaselineForPatchGroup)

-- | The name of the patch group whose patch baseline should be retrieved.
getPatchBaselineForPatchGroup_patchGroup :: Lens.Lens' GetPatchBaselineForPatchGroup Prelude.Text
getPatchBaselineForPatchGroup_patchGroup = Lens.lens (\GetPatchBaselineForPatchGroup' {patchGroup} -> patchGroup) (\s@GetPatchBaselineForPatchGroup' {} a -> s {patchGroup = a} :: GetPatchBaselineForPatchGroup)

instance
  Core.AWSRequest
    GetPatchBaselineForPatchGroup
  where
  type
    AWSResponse GetPatchBaselineForPatchGroup =
      GetPatchBaselineForPatchGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPatchBaselineForPatchGroupResponse'
            Prelude.<$> (x Core..?> "BaselineId")
            Prelude.<*> (x Core..?> "PatchGroup")
            Prelude.<*> (x Core..?> "OperatingSystem")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetPatchBaselineForPatchGroup

instance Prelude.NFData GetPatchBaselineForPatchGroup

instance Core.ToHeaders GetPatchBaselineForPatchGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.GetPatchBaselineForPatchGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetPatchBaselineForPatchGroup where
  toJSON GetPatchBaselineForPatchGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("OperatingSystem" Core..=)
              Prelude.<$> operatingSystem,
            Prelude.Just ("PatchGroup" Core..= patchGroup)
          ]
      )

instance Core.ToPath GetPatchBaselineForPatchGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery GetPatchBaselineForPatchGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPatchBaselineForPatchGroupResponse' smart constructor.
data GetPatchBaselineForPatchGroupResponse = GetPatchBaselineForPatchGroupResponse'
  { -- | The ID of the patch baseline that should be used for the patch group.
    baselineId :: Prelude.Maybe Prelude.Text,
    -- | The name of the patch group.
    patchGroup :: Prelude.Maybe Prelude.Text,
    -- | The operating system rule specified for patch groups using the patch
    -- baseline.
    operatingSystem :: Prelude.Maybe OperatingSystem,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPatchBaselineForPatchGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baselineId', 'getPatchBaselineForPatchGroupResponse_baselineId' - The ID of the patch baseline that should be used for the patch group.
--
-- 'patchGroup', 'getPatchBaselineForPatchGroupResponse_patchGroup' - The name of the patch group.
--
-- 'operatingSystem', 'getPatchBaselineForPatchGroupResponse_operatingSystem' - The operating system rule specified for patch groups using the patch
-- baseline.
--
-- 'httpStatus', 'getPatchBaselineForPatchGroupResponse_httpStatus' - The response's http status code.
newGetPatchBaselineForPatchGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPatchBaselineForPatchGroupResponse
newGetPatchBaselineForPatchGroupResponse pHttpStatus_ =
  GetPatchBaselineForPatchGroupResponse'
    { baselineId =
        Prelude.Nothing,
      patchGroup = Prelude.Nothing,
      operatingSystem = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the patch baseline that should be used for the patch group.
getPatchBaselineForPatchGroupResponse_baselineId :: Lens.Lens' GetPatchBaselineForPatchGroupResponse (Prelude.Maybe Prelude.Text)
getPatchBaselineForPatchGroupResponse_baselineId = Lens.lens (\GetPatchBaselineForPatchGroupResponse' {baselineId} -> baselineId) (\s@GetPatchBaselineForPatchGroupResponse' {} a -> s {baselineId = a} :: GetPatchBaselineForPatchGroupResponse)

-- | The name of the patch group.
getPatchBaselineForPatchGroupResponse_patchGroup :: Lens.Lens' GetPatchBaselineForPatchGroupResponse (Prelude.Maybe Prelude.Text)
getPatchBaselineForPatchGroupResponse_patchGroup = Lens.lens (\GetPatchBaselineForPatchGroupResponse' {patchGroup} -> patchGroup) (\s@GetPatchBaselineForPatchGroupResponse' {} a -> s {patchGroup = a} :: GetPatchBaselineForPatchGroupResponse)

-- | The operating system rule specified for patch groups using the patch
-- baseline.
getPatchBaselineForPatchGroupResponse_operatingSystem :: Lens.Lens' GetPatchBaselineForPatchGroupResponse (Prelude.Maybe OperatingSystem)
getPatchBaselineForPatchGroupResponse_operatingSystem = Lens.lens (\GetPatchBaselineForPatchGroupResponse' {operatingSystem} -> operatingSystem) (\s@GetPatchBaselineForPatchGroupResponse' {} a -> s {operatingSystem = a} :: GetPatchBaselineForPatchGroupResponse)

-- | The response's http status code.
getPatchBaselineForPatchGroupResponse_httpStatus :: Lens.Lens' GetPatchBaselineForPatchGroupResponse Prelude.Int
getPatchBaselineForPatchGroupResponse_httpStatus = Lens.lens (\GetPatchBaselineForPatchGroupResponse' {httpStatus} -> httpStatus) (\s@GetPatchBaselineForPatchGroupResponse' {} a -> s {httpStatus = a} :: GetPatchBaselineForPatchGroupResponse)

instance
  Prelude.NFData
    GetPatchBaselineForPatchGroupResponse
