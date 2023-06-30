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
-- Module      : Amazonka.SSM.GetPatchBaselineForPatchGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the patch baseline that should be used for the specified patch
-- group.
module Amazonka.SSM.GetPatchBaselineForPatchGroup
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
    getPatchBaselineForPatchGroupResponse_operatingSystem,
    getPatchBaselineForPatchGroupResponse_patchGroup,
    getPatchBaselineForPatchGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newGetPatchBaselineForPatchGroup' smart constructor.
data GetPatchBaselineForPatchGroup = GetPatchBaselineForPatchGroup'
  { -- | Returns the operating system rule specified for patch groups using the
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
-- 'operatingSystem', 'getPatchBaselineForPatchGroup_operatingSystem' - Returns the operating system rule specified for patch groups using the
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

-- | Returns the operating system rule specified for patch groups using the
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPatchBaselineForPatchGroupResponse'
            Prelude.<$> (x Data..?> "BaselineId")
            Prelude.<*> (x Data..?> "OperatingSystem")
            Prelude.<*> (x Data..?> "PatchGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetPatchBaselineForPatchGroup
  where
  hashWithSalt _salt GetPatchBaselineForPatchGroup' {..} =
    _salt
      `Prelude.hashWithSalt` operatingSystem
      `Prelude.hashWithSalt` patchGroup

instance Prelude.NFData GetPatchBaselineForPatchGroup where
  rnf GetPatchBaselineForPatchGroup' {..} =
    Prelude.rnf operatingSystem
      `Prelude.seq` Prelude.rnf patchGroup

instance Data.ToHeaders GetPatchBaselineForPatchGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.GetPatchBaselineForPatchGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetPatchBaselineForPatchGroup where
  toJSON GetPatchBaselineForPatchGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("OperatingSystem" Data..=)
              Prelude.<$> operatingSystem,
            Prelude.Just ("PatchGroup" Data..= patchGroup)
          ]
      )

instance Data.ToPath GetPatchBaselineForPatchGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery GetPatchBaselineForPatchGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPatchBaselineForPatchGroupResponse' smart constructor.
data GetPatchBaselineForPatchGroupResponse = GetPatchBaselineForPatchGroupResponse'
  { -- | The ID of the patch baseline that should be used for the patch group.
    baselineId :: Prelude.Maybe Prelude.Text,
    -- | The operating system rule specified for patch groups using the patch
    -- baseline.
    operatingSystem :: Prelude.Maybe OperatingSystem,
    -- | The name of the patch group.
    patchGroup :: Prelude.Maybe Prelude.Text,
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
-- 'operatingSystem', 'getPatchBaselineForPatchGroupResponse_operatingSystem' - The operating system rule specified for patch groups using the patch
-- baseline.
--
-- 'patchGroup', 'getPatchBaselineForPatchGroupResponse_patchGroup' - The name of the patch group.
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
      operatingSystem = Prelude.Nothing,
      patchGroup = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the patch baseline that should be used for the patch group.
getPatchBaselineForPatchGroupResponse_baselineId :: Lens.Lens' GetPatchBaselineForPatchGroupResponse (Prelude.Maybe Prelude.Text)
getPatchBaselineForPatchGroupResponse_baselineId = Lens.lens (\GetPatchBaselineForPatchGroupResponse' {baselineId} -> baselineId) (\s@GetPatchBaselineForPatchGroupResponse' {} a -> s {baselineId = a} :: GetPatchBaselineForPatchGroupResponse)

-- | The operating system rule specified for patch groups using the patch
-- baseline.
getPatchBaselineForPatchGroupResponse_operatingSystem :: Lens.Lens' GetPatchBaselineForPatchGroupResponse (Prelude.Maybe OperatingSystem)
getPatchBaselineForPatchGroupResponse_operatingSystem = Lens.lens (\GetPatchBaselineForPatchGroupResponse' {operatingSystem} -> operatingSystem) (\s@GetPatchBaselineForPatchGroupResponse' {} a -> s {operatingSystem = a} :: GetPatchBaselineForPatchGroupResponse)

-- | The name of the patch group.
getPatchBaselineForPatchGroupResponse_patchGroup :: Lens.Lens' GetPatchBaselineForPatchGroupResponse (Prelude.Maybe Prelude.Text)
getPatchBaselineForPatchGroupResponse_patchGroup = Lens.lens (\GetPatchBaselineForPatchGroupResponse' {patchGroup} -> patchGroup) (\s@GetPatchBaselineForPatchGroupResponse' {} a -> s {patchGroup = a} :: GetPatchBaselineForPatchGroupResponse)

-- | The response's http status code.
getPatchBaselineForPatchGroupResponse_httpStatus :: Lens.Lens' GetPatchBaselineForPatchGroupResponse Prelude.Int
getPatchBaselineForPatchGroupResponse_httpStatus = Lens.lens (\GetPatchBaselineForPatchGroupResponse' {httpStatus} -> httpStatus) (\s@GetPatchBaselineForPatchGroupResponse' {} a -> s {httpStatus = a} :: GetPatchBaselineForPatchGroupResponse)

instance
  Prelude.NFData
    GetPatchBaselineForPatchGroupResponse
  where
  rnf GetPatchBaselineForPatchGroupResponse' {..} =
    Prelude.rnf baselineId
      `Prelude.seq` Prelude.rnf operatingSystem
      `Prelude.seq` Prelude.rnf patchGroup
      `Prelude.seq` Prelude.rnf httpStatus
