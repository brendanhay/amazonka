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
-- Module      : Amazonka.SSM.RegisterDefaultPatchBaseline
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Defines the default patch baseline for the relevant operating system.
--
-- To reset the Amazon Web Services-predefined patch baseline as the
-- default, specify the full patch baseline Amazon Resource Name (ARN) as
-- the baseline ID value. For example, for CentOS, specify
-- @arn:aws:ssm:us-east-2:733109147000:patchbaseline\/pb-0574b43a65ea646ed@
-- instead of @pb-0574b43a65ea646ed@.
module Amazonka.SSM.RegisterDefaultPatchBaseline
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newRegisterDefaultPatchBaseline' smart constructor.
data RegisterDefaultPatchBaseline = RegisterDefaultPatchBaseline'
  { -- | The ID of the patch baseline that should be the default patch baseline.
    baselineId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  RegisterDefaultPatchBaseline
newRegisterDefaultPatchBaseline pBaselineId_ =
  RegisterDefaultPatchBaseline'
    { baselineId =
        pBaselineId_
    }

-- | The ID of the patch baseline that should be the default patch baseline.
registerDefaultPatchBaseline_baselineId :: Lens.Lens' RegisterDefaultPatchBaseline Prelude.Text
registerDefaultPatchBaseline_baselineId = Lens.lens (\RegisterDefaultPatchBaseline' {baselineId} -> baselineId) (\s@RegisterDefaultPatchBaseline' {} a -> s {baselineId = a} :: RegisterDefaultPatchBaseline)

instance Core.AWSRequest RegisterDefaultPatchBaseline where
  type
    AWSResponse RegisterDefaultPatchBaseline =
      RegisterDefaultPatchBaselineResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterDefaultPatchBaselineResponse'
            Prelude.<$> (x Core..?> "BaselineId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RegisterDefaultPatchBaseline
  where
  hashWithSalt _salt RegisterDefaultPatchBaseline' {..} =
    _salt `Prelude.hashWithSalt` baselineId

instance Prelude.NFData RegisterDefaultPatchBaseline where
  rnf RegisterDefaultPatchBaseline' {..} =
    Prelude.rnf baselineId

instance Core.ToHeaders RegisterDefaultPatchBaseline where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.RegisterDefaultPatchBaseline" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RegisterDefaultPatchBaseline where
  toJSON RegisterDefaultPatchBaseline' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("BaselineId" Core..= baselineId)]
      )

instance Core.ToPath RegisterDefaultPatchBaseline where
  toPath = Prelude.const "/"

instance Core.ToQuery RegisterDefaultPatchBaseline where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterDefaultPatchBaselineResponse' smart constructor.
data RegisterDefaultPatchBaselineResponse = RegisterDefaultPatchBaselineResponse'
  { -- | The ID of the default patch baseline.
    baselineId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  RegisterDefaultPatchBaselineResponse
newRegisterDefaultPatchBaselineResponse pHttpStatus_ =
  RegisterDefaultPatchBaselineResponse'
    { baselineId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the default patch baseline.
registerDefaultPatchBaselineResponse_baselineId :: Lens.Lens' RegisterDefaultPatchBaselineResponse (Prelude.Maybe Prelude.Text)
registerDefaultPatchBaselineResponse_baselineId = Lens.lens (\RegisterDefaultPatchBaselineResponse' {baselineId} -> baselineId) (\s@RegisterDefaultPatchBaselineResponse' {} a -> s {baselineId = a} :: RegisterDefaultPatchBaselineResponse)

-- | The response's http status code.
registerDefaultPatchBaselineResponse_httpStatus :: Lens.Lens' RegisterDefaultPatchBaselineResponse Prelude.Int
registerDefaultPatchBaselineResponse_httpStatus = Lens.lens (\RegisterDefaultPatchBaselineResponse' {httpStatus} -> httpStatus) (\s@RegisterDefaultPatchBaselineResponse' {} a -> s {httpStatus = a} :: RegisterDefaultPatchBaselineResponse)

instance
  Prelude.NFData
    RegisterDefaultPatchBaselineResponse
  where
  rnf RegisterDefaultPatchBaselineResponse' {..} =
    Prelude.rnf baselineId
      `Prelude.seq` Prelude.rnf httpStatus
