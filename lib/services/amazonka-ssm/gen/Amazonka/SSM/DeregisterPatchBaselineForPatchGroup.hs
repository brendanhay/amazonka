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
-- Module      : Amazonka.SSM.DeregisterPatchBaselineForPatchGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a patch group from a patch baseline.
module Amazonka.SSM.DeregisterPatchBaselineForPatchGroup
  ( -- * Creating a Request
    DeregisterPatchBaselineForPatchGroup (..),
    newDeregisterPatchBaselineForPatchGroup,

    -- * Request Lenses
    deregisterPatchBaselineForPatchGroup_baselineId,
    deregisterPatchBaselineForPatchGroup_patchGroup,

    -- * Destructuring the Response
    DeregisterPatchBaselineForPatchGroupResponse (..),
    newDeregisterPatchBaselineForPatchGroupResponse,

    -- * Response Lenses
    deregisterPatchBaselineForPatchGroupResponse_baselineId,
    deregisterPatchBaselineForPatchGroupResponse_patchGroup,
    deregisterPatchBaselineForPatchGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDeregisterPatchBaselineForPatchGroup' smart constructor.
data DeregisterPatchBaselineForPatchGroup = DeregisterPatchBaselineForPatchGroup'
  { -- | The ID of the patch baseline to deregister the patch group from.
    baselineId :: Prelude.Text,
    -- | The name of the patch group that should be deregistered from the patch
    -- baseline.
    patchGroup :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterPatchBaselineForPatchGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baselineId', 'deregisterPatchBaselineForPatchGroup_baselineId' - The ID of the patch baseline to deregister the patch group from.
--
-- 'patchGroup', 'deregisterPatchBaselineForPatchGroup_patchGroup' - The name of the patch group that should be deregistered from the patch
-- baseline.
newDeregisterPatchBaselineForPatchGroup ::
  -- | 'baselineId'
  Prelude.Text ->
  -- | 'patchGroup'
  Prelude.Text ->
  DeregisterPatchBaselineForPatchGroup
newDeregisterPatchBaselineForPatchGroup
  pBaselineId_
  pPatchGroup_ =
    DeregisterPatchBaselineForPatchGroup'
      { baselineId =
          pBaselineId_,
        patchGroup = pPatchGroup_
      }

-- | The ID of the patch baseline to deregister the patch group from.
deregisterPatchBaselineForPatchGroup_baselineId :: Lens.Lens' DeregisterPatchBaselineForPatchGroup Prelude.Text
deregisterPatchBaselineForPatchGroup_baselineId = Lens.lens (\DeregisterPatchBaselineForPatchGroup' {baselineId} -> baselineId) (\s@DeregisterPatchBaselineForPatchGroup' {} a -> s {baselineId = a} :: DeregisterPatchBaselineForPatchGroup)

-- | The name of the patch group that should be deregistered from the patch
-- baseline.
deregisterPatchBaselineForPatchGroup_patchGroup :: Lens.Lens' DeregisterPatchBaselineForPatchGroup Prelude.Text
deregisterPatchBaselineForPatchGroup_patchGroup = Lens.lens (\DeregisterPatchBaselineForPatchGroup' {patchGroup} -> patchGroup) (\s@DeregisterPatchBaselineForPatchGroup' {} a -> s {patchGroup = a} :: DeregisterPatchBaselineForPatchGroup)

instance
  Core.AWSRequest
    DeregisterPatchBaselineForPatchGroup
  where
  type
    AWSResponse DeregisterPatchBaselineForPatchGroup =
      DeregisterPatchBaselineForPatchGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeregisterPatchBaselineForPatchGroupResponse'
            Prelude.<$> (x Core..?> "BaselineId")
              Prelude.<*> (x Core..?> "PatchGroup")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeregisterPatchBaselineForPatchGroup
  where
  hashWithSalt
    _salt
    DeregisterPatchBaselineForPatchGroup' {..} =
      _salt `Prelude.hashWithSalt` baselineId
        `Prelude.hashWithSalt` patchGroup

instance
  Prelude.NFData
    DeregisterPatchBaselineForPatchGroup
  where
  rnf DeregisterPatchBaselineForPatchGroup' {..} =
    Prelude.rnf baselineId
      `Prelude.seq` Prelude.rnf patchGroup

instance
  Core.ToHeaders
    DeregisterPatchBaselineForPatchGroup
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DeregisterPatchBaselineForPatchGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    DeregisterPatchBaselineForPatchGroup
  where
  toJSON DeregisterPatchBaselineForPatchGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("BaselineId" Core..= baselineId),
            Prelude.Just ("PatchGroup" Core..= patchGroup)
          ]
      )

instance
  Core.ToPath
    DeregisterPatchBaselineForPatchGroup
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DeregisterPatchBaselineForPatchGroup
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterPatchBaselineForPatchGroupResponse' smart constructor.
data DeregisterPatchBaselineForPatchGroupResponse = DeregisterPatchBaselineForPatchGroupResponse'
  { -- | The ID of the patch baseline the patch group was deregistered from.
    baselineId :: Prelude.Maybe Prelude.Text,
    -- | The name of the patch group deregistered from the patch baseline.
    patchGroup :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterPatchBaselineForPatchGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baselineId', 'deregisterPatchBaselineForPatchGroupResponse_baselineId' - The ID of the patch baseline the patch group was deregistered from.
--
-- 'patchGroup', 'deregisterPatchBaselineForPatchGroupResponse_patchGroup' - The name of the patch group deregistered from the patch baseline.
--
-- 'httpStatus', 'deregisterPatchBaselineForPatchGroupResponse_httpStatus' - The response's http status code.
newDeregisterPatchBaselineForPatchGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeregisterPatchBaselineForPatchGroupResponse
newDeregisterPatchBaselineForPatchGroupResponse
  pHttpStatus_ =
    DeregisterPatchBaselineForPatchGroupResponse'
      { baselineId =
          Prelude.Nothing,
        patchGroup = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ID of the patch baseline the patch group was deregistered from.
deregisterPatchBaselineForPatchGroupResponse_baselineId :: Lens.Lens' DeregisterPatchBaselineForPatchGroupResponse (Prelude.Maybe Prelude.Text)
deregisterPatchBaselineForPatchGroupResponse_baselineId = Lens.lens (\DeregisterPatchBaselineForPatchGroupResponse' {baselineId} -> baselineId) (\s@DeregisterPatchBaselineForPatchGroupResponse' {} a -> s {baselineId = a} :: DeregisterPatchBaselineForPatchGroupResponse)

-- | The name of the patch group deregistered from the patch baseline.
deregisterPatchBaselineForPatchGroupResponse_patchGroup :: Lens.Lens' DeregisterPatchBaselineForPatchGroupResponse (Prelude.Maybe Prelude.Text)
deregisterPatchBaselineForPatchGroupResponse_patchGroup = Lens.lens (\DeregisterPatchBaselineForPatchGroupResponse' {patchGroup} -> patchGroup) (\s@DeregisterPatchBaselineForPatchGroupResponse' {} a -> s {patchGroup = a} :: DeregisterPatchBaselineForPatchGroupResponse)

-- | The response's http status code.
deregisterPatchBaselineForPatchGroupResponse_httpStatus :: Lens.Lens' DeregisterPatchBaselineForPatchGroupResponse Prelude.Int
deregisterPatchBaselineForPatchGroupResponse_httpStatus = Lens.lens (\DeregisterPatchBaselineForPatchGroupResponse' {httpStatus} -> httpStatus) (\s@DeregisterPatchBaselineForPatchGroupResponse' {} a -> s {httpStatus = a} :: DeregisterPatchBaselineForPatchGroupResponse)

instance
  Prelude.NFData
    DeregisterPatchBaselineForPatchGroupResponse
  where
  rnf DeregisterPatchBaselineForPatchGroupResponse' {..} =
    Prelude.rnf baselineId
      `Prelude.seq` Prelude.rnf patchGroup
      `Prelude.seq` Prelude.rnf httpStatus
