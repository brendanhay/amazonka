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
-- Module      : Network.AWS.Snowball.CreateReturnShippingLabel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a shipping label that will be used to return the Snow device to
-- AWS.
module Network.AWS.Snowball.CreateReturnShippingLabel
  ( -- * Creating a Request
    CreateReturnShippingLabel (..),
    newCreateReturnShippingLabel,

    -- * Request Lenses
    createReturnShippingLabel_shippingOption,
    createReturnShippingLabel_jobId,

    -- * Destructuring the Response
    CreateReturnShippingLabelResponse (..),
    newCreateReturnShippingLabelResponse,

    -- * Response Lenses
    createReturnShippingLabelResponse_status,
    createReturnShippingLabelResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Snowball.Types

-- | /See:/ 'newCreateReturnShippingLabel' smart constructor.
data CreateReturnShippingLabel = CreateReturnShippingLabel'
  { -- | The shipping speed for a particular job. This speed doesn\'t dictate how
    -- soon the device is returned to AWS. This speed represents how quickly it
    -- moves to its destination while in transit. Regional shipping speeds are
    -- as follows:
    shippingOption :: Prelude.Maybe ShippingOption,
    -- | The ID for a job that you want to create the return shipping label for.
    -- For example @JID123e4567-e89b-12d3-a456-426655440000@.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateReturnShippingLabel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shippingOption', 'createReturnShippingLabel_shippingOption' - The shipping speed for a particular job. This speed doesn\'t dictate how
-- soon the device is returned to AWS. This speed represents how quickly it
-- moves to its destination while in transit. Regional shipping speeds are
-- as follows:
--
-- 'jobId', 'createReturnShippingLabel_jobId' - The ID for a job that you want to create the return shipping label for.
-- For example @JID123e4567-e89b-12d3-a456-426655440000@.
newCreateReturnShippingLabel ::
  -- | 'jobId'
  Prelude.Text ->
  CreateReturnShippingLabel
newCreateReturnShippingLabel pJobId_ =
  CreateReturnShippingLabel'
    { shippingOption =
        Prelude.Nothing,
      jobId = pJobId_
    }

-- | The shipping speed for a particular job. This speed doesn\'t dictate how
-- soon the device is returned to AWS. This speed represents how quickly it
-- moves to its destination while in transit. Regional shipping speeds are
-- as follows:
createReturnShippingLabel_shippingOption :: Lens.Lens' CreateReturnShippingLabel (Prelude.Maybe ShippingOption)
createReturnShippingLabel_shippingOption = Lens.lens (\CreateReturnShippingLabel' {shippingOption} -> shippingOption) (\s@CreateReturnShippingLabel' {} a -> s {shippingOption = a} :: CreateReturnShippingLabel)

-- | The ID for a job that you want to create the return shipping label for.
-- For example @JID123e4567-e89b-12d3-a456-426655440000@.
createReturnShippingLabel_jobId :: Lens.Lens' CreateReturnShippingLabel Prelude.Text
createReturnShippingLabel_jobId = Lens.lens (\CreateReturnShippingLabel' {jobId} -> jobId) (\s@CreateReturnShippingLabel' {} a -> s {jobId = a} :: CreateReturnShippingLabel)

instance Core.AWSRequest CreateReturnShippingLabel where
  type
    AWSResponse CreateReturnShippingLabel =
      CreateReturnShippingLabelResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateReturnShippingLabelResponse'
            Prelude.<$> (x Core..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateReturnShippingLabel

instance Prelude.NFData CreateReturnShippingLabel

instance Core.ToHeaders CreateReturnShippingLabel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSIESnowballJobManagementService.CreateReturnShippingLabel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateReturnShippingLabel where
  toJSON CreateReturnShippingLabel' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ShippingOption" Core..=)
              Prelude.<$> shippingOption,
            Prelude.Just ("JobId" Core..= jobId)
          ]
      )

instance Core.ToPath CreateReturnShippingLabel where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateReturnShippingLabel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateReturnShippingLabelResponse' smart constructor.
data CreateReturnShippingLabelResponse = CreateReturnShippingLabelResponse'
  { -- | The status information of the task on a Snow device that is being
    -- returned to AWS.
    status :: Prelude.Maybe ShippingLabelStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateReturnShippingLabelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'createReturnShippingLabelResponse_status' - The status information of the task on a Snow device that is being
-- returned to AWS.
--
-- 'httpStatus', 'createReturnShippingLabelResponse_httpStatus' - The response's http status code.
newCreateReturnShippingLabelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateReturnShippingLabelResponse
newCreateReturnShippingLabelResponse pHttpStatus_ =
  CreateReturnShippingLabelResponse'
    { status =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status information of the task on a Snow device that is being
-- returned to AWS.
createReturnShippingLabelResponse_status :: Lens.Lens' CreateReturnShippingLabelResponse (Prelude.Maybe ShippingLabelStatus)
createReturnShippingLabelResponse_status = Lens.lens (\CreateReturnShippingLabelResponse' {status} -> status) (\s@CreateReturnShippingLabelResponse' {} a -> s {status = a} :: CreateReturnShippingLabelResponse)

-- | The response's http status code.
createReturnShippingLabelResponse_httpStatus :: Lens.Lens' CreateReturnShippingLabelResponse Prelude.Int
createReturnShippingLabelResponse_httpStatus = Lens.lens (\CreateReturnShippingLabelResponse' {httpStatus} -> httpStatus) (\s@CreateReturnShippingLabelResponse' {} a -> s {httpStatus = a} :: CreateReturnShippingLabelResponse)

instance
  Prelude.NFData
    CreateReturnShippingLabelResponse
