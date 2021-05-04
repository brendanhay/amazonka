{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SSM.RegisterPatchBaselineForPatchGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a patch baseline for a patch group.
module Network.AWS.SSM.RegisterPatchBaselineForPatchGroup
  ( -- * Creating a Request
    RegisterPatchBaselineForPatchGroup (..),
    newRegisterPatchBaselineForPatchGroup,

    -- * Request Lenses
    registerPatchBaselineForPatchGroup_baselineId,
    registerPatchBaselineForPatchGroup_patchGroup,

    -- * Destructuring the Response
    RegisterPatchBaselineForPatchGroupResponse (..),
    newRegisterPatchBaselineForPatchGroupResponse,

    -- * Response Lenses
    registerPatchBaselineForPatchGroupResponse_baselineId,
    registerPatchBaselineForPatchGroupResponse_patchGroup,
    registerPatchBaselineForPatchGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newRegisterPatchBaselineForPatchGroup' smart constructor.
data RegisterPatchBaselineForPatchGroup = RegisterPatchBaselineForPatchGroup'
  { -- | The ID of the patch baseline to register the patch group with.
    baselineId :: Prelude.Text,
    -- | The name of the patch group that should be registered with the patch
    -- baseline.
    patchGroup :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RegisterPatchBaselineForPatchGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baselineId', 'registerPatchBaselineForPatchGroup_baselineId' - The ID of the patch baseline to register the patch group with.
--
-- 'patchGroup', 'registerPatchBaselineForPatchGroup_patchGroup' - The name of the patch group that should be registered with the patch
-- baseline.
newRegisterPatchBaselineForPatchGroup ::
  -- | 'baselineId'
  Prelude.Text ->
  -- | 'patchGroup'
  Prelude.Text ->
  RegisterPatchBaselineForPatchGroup
newRegisterPatchBaselineForPatchGroup
  pBaselineId_
  pPatchGroup_ =
    RegisterPatchBaselineForPatchGroup'
      { baselineId =
          pBaselineId_,
        patchGroup = pPatchGroup_
      }

-- | The ID of the patch baseline to register the patch group with.
registerPatchBaselineForPatchGroup_baselineId :: Lens.Lens' RegisterPatchBaselineForPatchGroup Prelude.Text
registerPatchBaselineForPatchGroup_baselineId = Lens.lens (\RegisterPatchBaselineForPatchGroup' {baselineId} -> baselineId) (\s@RegisterPatchBaselineForPatchGroup' {} a -> s {baselineId = a} :: RegisterPatchBaselineForPatchGroup)

-- | The name of the patch group that should be registered with the patch
-- baseline.
registerPatchBaselineForPatchGroup_patchGroup :: Lens.Lens' RegisterPatchBaselineForPatchGroup Prelude.Text
registerPatchBaselineForPatchGroup_patchGroup = Lens.lens (\RegisterPatchBaselineForPatchGroup' {patchGroup} -> patchGroup) (\s@RegisterPatchBaselineForPatchGroup' {} a -> s {patchGroup = a} :: RegisterPatchBaselineForPatchGroup)

instance
  Prelude.AWSRequest
    RegisterPatchBaselineForPatchGroup
  where
  type
    Rs RegisterPatchBaselineForPatchGroup =
      RegisterPatchBaselineForPatchGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterPatchBaselineForPatchGroupResponse'
            Prelude.<$> (x Prelude..?> "BaselineId")
              Prelude.<*> (x Prelude..?> "PatchGroup")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RegisterPatchBaselineForPatchGroup

instance
  Prelude.NFData
    RegisterPatchBaselineForPatchGroup

instance
  Prelude.ToHeaders
    RegisterPatchBaselineForPatchGroup
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonSSM.RegisterPatchBaselineForPatchGroup" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    RegisterPatchBaselineForPatchGroup
  where
  toJSON RegisterPatchBaselineForPatchGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("BaselineId" Prelude..= baselineId),
            Prelude.Just ("PatchGroup" Prelude..= patchGroup)
          ]
      )

instance
  Prelude.ToPath
    RegisterPatchBaselineForPatchGroup
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    RegisterPatchBaselineForPatchGroup
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterPatchBaselineForPatchGroupResponse' smart constructor.
data RegisterPatchBaselineForPatchGroupResponse = RegisterPatchBaselineForPatchGroupResponse'
  { -- | The ID of the patch baseline the patch group was registered with.
    baselineId :: Prelude.Maybe Prelude.Text,
    -- | The name of the patch group registered with the patch baseline.
    patchGroup :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RegisterPatchBaselineForPatchGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baselineId', 'registerPatchBaselineForPatchGroupResponse_baselineId' - The ID of the patch baseline the patch group was registered with.
--
-- 'patchGroup', 'registerPatchBaselineForPatchGroupResponse_patchGroup' - The name of the patch group registered with the patch baseline.
--
-- 'httpStatus', 'registerPatchBaselineForPatchGroupResponse_httpStatus' - The response's http status code.
newRegisterPatchBaselineForPatchGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterPatchBaselineForPatchGroupResponse
newRegisterPatchBaselineForPatchGroupResponse
  pHttpStatus_ =
    RegisterPatchBaselineForPatchGroupResponse'
      { baselineId =
          Prelude.Nothing,
        patchGroup = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ID of the patch baseline the patch group was registered with.
registerPatchBaselineForPatchGroupResponse_baselineId :: Lens.Lens' RegisterPatchBaselineForPatchGroupResponse (Prelude.Maybe Prelude.Text)
registerPatchBaselineForPatchGroupResponse_baselineId = Lens.lens (\RegisterPatchBaselineForPatchGroupResponse' {baselineId} -> baselineId) (\s@RegisterPatchBaselineForPatchGroupResponse' {} a -> s {baselineId = a} :: RegisterPatchBaselineForPatchGroupResponse)

-- | The name of the patch group registered with the patch baseline.
registerPatchBaselineForPatchGroupResponse_patchGroup :: Lens.Lens' RegisterPatchBaselineForPatchGroupResponse (Prelude.Maybe Prelude.Text)
registerPatchBaselineForPatchGroupResponse_patchGroup = Lens.lens (\RegisterPatchBaselineForPatchGroupResponse' {patchGroup} -> patchGroup) (\s@RegisterPatchBaselineForPatchGroupResponse' {} a -> s {patchGroup = a} :: RegisterPatchBaselineForPatchGroupResponse)

-- | The response's http status code.
registerPatchBaselineForPatchGroupResponse_httpStatus :: Lens.Lens' RegisterPatchBaselineForPatchGroupResponse Prelude.Int
registerPatchBaselineForPatchGroupResponse_httpStatus = Lens.lens (\RegisterPatchBaselineForPatchGroupResponse' {httpStatus} -> httpStatus) (\s@RegisterPatchBaselineForPatchGroupResponse' {} a -> s {httpStatus = a} :: RegisterPatchBaselineForPatchGroupResponse)

instance
  Prelude.NFData
    RegisterPatchBaselineForPatchGroupResponse
