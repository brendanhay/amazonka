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
-- Module      : Network.AWS.SSM.RegisterDefaultPatchBaseline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Defines the default patch baseline for the relevant operating system.
--
-- To reset the AWS predefined patch baseline as the default, specify the
-- full patch baseline ARN as the baseline ID value. For example, for
-- CentOS, specify
-- @arn:aws:ssm:us-east-2:733109147000:patchbaseline\/pb-0574b43a65ea646ed@
-- instead of @pb-0574b43a65ea646ed@.
module Network.AWS.SSM.RegisterDefaultPatchBaseline
  ( -- * Creating a Request
    RegisterDefaultPatchBaseline (..),
    newRegisterDefaultPatchBaseline,

    -- * Request Lenses
    registerDefaultPatchBaseline_baselineId,

    -- * Destructuring the Response
    RegisterDefaultPatchBaselineResponse (..),
    newRegisterDefaultPatchBaselineResponse,

    -- * Response Lenses
    registerDefaultPatchBaselineResponse_baselineId,
    registerDefaultPatchBaselineResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newRegisterDefaultPatchBaseline' smart constructor.
data RegisterDefaultPatchBaseline = RegisterDefaultPatchBaseline'
  { -- | The ID of the patch baseline that should be the default patch baseline.
    baselineId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterDefaultPatchBaseline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baselineId', 'registerDefaultPatchBaseline_baselineId' - The ID of the patch baseline that should be the default patch baseline.
newRegisterDefaultPatchBaseline ::
  -- | 'baselineId'
  Core.Text ->
  RegisterDefaultPatchBaseline
newRegisterDefaultPatchBaseline pBaselineId_ =
  RegisterDefaultPatchBaseline'
    { baselineId =
        pBaselineId_
    }

-- | The ID of the patch baseline that should be the default patch baseline.
registerDefaultPatchBaseline_baselineId :: Lens.Lens' RegisterDefaultPatchBaseline Core.Text
registerDefaultPatchBaseline_baselineId = Lens.lens (\RegisterDefaultPatchBaseline' {baselineId} -> baselineId) (\s@RegisterDefaultPatchBaseline' {} a -> s {baselineId = a} :: RegisterDefaultPatchBaseline)

instance Core.AWSRequest RegisterDefaultPatchBaseline where
  type
    AWSResponse RegisterDefaultPatchBaseline =
      RegisterDefaultPatchBaselineResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterDefaultPatchBaselineResponse'
            Core.<$> (x Core..?> "BaselineId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RegisterDefaultPatchBaseline

instance Core.NFData RegisterDefaultPatchBaseline

instance Core.ToHeaders RegisterDefaultPatchBaseline where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.RegisterDefaultPatchBaseline" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RegisterDefaultPatchBaseline where
  toJSON RegisterDefaultPatchBaseline' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("BaselineId" Core..= baselineId)]
      )

instance Core.ToPath RegisterDefaultPatchBaseline where
  toPath = Core.const "/"

instance Core.ToQuery RegisterDefaultPatchBaseline where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRegisterDefaultPatchBaselineResponse' smart constructor.
data RegisterDefaultPatchBaselineResponse = RegisterDefaultPatchBaselineResponse'
  { -- | The ID of the default patch baseline.
    baselineId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterDefaultPatchBaselineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baselineId', 'registerDefaultPatchBaselineResponse_baselineId' - The ID of the default patch baseline.
--
-- 'httpStatus', 'registerDefaultPatchBaselineResponse_httpStatus' - The response's http status code.
newRegisterDefaultPatchBaselineResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RegisterDefaultPatchBaselineResponse
newRegisterDefaultPatchBaselineResponse pHttpStatus_ =
  RegisterDefaultPatchBaselineResponse'
    { baselineId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the default patch baseline.
registerDefaultPatchBaselineResponse_baselineId :: Lens.Lens' RegisterDefaultPatchBaselineResponse (Core.Maybe Core.Text)
registerDefaultPatchBaselineResponse_baselineId = Lens.lens (\RegisterDefaultPatchBaselineResponse' {baselineId} -> baselineId) (\s@RegisterDefaultPatchBaselineResponse' {} a -> s {baselineId = a} :: RegisterDefaultPatchBaselineResponse)

-- | The response's http status code.
registerDefaultPatchBaselineResponse_httpStatus :: Lens.Lens' RegisterDefaultPatchBaselineResponse Core.Int
registerDefaultPatchBaselineResponse_httpStatus = Lens.lens (\RegisterDefaultPatchBaselineResponse' {httpStatus} -> httpStatus) (\s@RegisterDefaultPatchBaselineResponse' {} a -> s {httpStatus = a} :: RegisterDefaultPatchBaselineResponse)

instance
  Core.NFData
    RegisterDefaultPatchBaselineResponse
