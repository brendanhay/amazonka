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
-- Module      : Network.AWS.CloudHSM.CreateHapg
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__. For more
-- information, see
-- <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs>,
-- the
-- <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide>,
-- and the
-- <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference>.
--
-- __For information about the current version of AWS CloudHSM__, see
-- <http://aws.amazon.com/cloudhsm/ AWS CloudHSM>, the
-- <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide>,
-- and the
-- <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference>.
--
-- Creates a high-availability partition group. A high-availability
-- partition group is a group of partitions that spans multiple physical
-- HSMs.
module Network.AWS.CloudHSM.CreateHapg
  ( -- * Creating a Request
    CreateHapg (..),
    newCreateHapg,

    -- * Request Lenses
    createHapg_label,

    -- * Destructuring the Response
    CreateHapgResponse (..),
    newCreateHapgResponse,

    -- * Response Lenses
    createHapgResponse_hapgArn,
    createHapgResponse_httpStatus,
  )
where

import Network.AWS.CloudHSM.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the CreateHapgRequest action.
--
-- /See:/ 'newCreateHapg' smart constructor.
data CreateHapg = CreateHapg'
  { -- | The label of the new high-availability partition group.
    label :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateHapg' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'label', 'createHapg_label' - The label of the new high-availability partition group.
newCreateHapg ::
  -- | 'label'
  Core.Text ->
  CreateHapg
newCreateHapg pLabel_ = CreateHapg' {label = pLabel_}

-- | The label of the new high-availability partition group.
createHapg_label :: Lens.Lens' CreateHapg Core.Text
createHapg_label = Lens.lens (\CreateHapg' {label} -> label) (\s@CreateHapg' {} a -> s {label = a} :: CreateHapg)

instance Core.AWSRequest CreateHapg where
  type AWSResponse CreateHapg = CreateHapgResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateHapgResponse'
            Core.<$> (x Core..?> "HapgArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateHapg

instance Core.NFData CreateHapg

instance Core.ToHeaders CreateHapg where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CloudHsmFrontendService.CreateHapg" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateHapg where
  toJSON CreateHapg' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Label" Core..= label)])

instance Core.ToPath CreateHapg where
  toPath = Core.const "/"

instance Core.ToQuery CreateHapg where
  toQuery = Core.const Core.mempty

-- | Contains the output of the CreateHAPartitionGroup action.
--
-- /See:/ 'newCreateHapgResponse' smart constructor.
data CreateHapgResponse = CreateHapgResponse'
  { -- | The ARN of the high-availability partition group.
    hapgArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateHapgResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hapgArn', 'createHapgResponse_hapgArn' - The ARN of the high-availability partition group.
--
-- 'httpStatus', 'createHapgResponse_httpStatus' - The response's http status code.
newCreateHapgResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateHapgResponse
newCreateHapgResponse pHttpStatus_ =
  CreateHapgResponse'
    { hapgArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the high-availability partition group.
createHapgResponse_hapgArn :: Lens.Lens' CreateHapgResponse (Core.Maybe Core.Text)
createHapgResponse_hapgArn = Lens.lens (\CreateHapgResponse' {hapgArn} -> hapgArn) (\s@CreateHapgResponse' {} a -> s {hapgArn = a} :: CreateHapgResponse)

-- | The response's http status code.
createHapgResponse_httpStatus :: Lens.Lens' CreateHapgResponse Core.Int
createHapgResponse_httpStatus = Lens.lens (\CreateHapgResponse' {httpStatus} -> httpStatus) (\s@CreateHapgResponse' {} a -> s {httpStatus = a} :: CreateHapgResponse)

instance Core.NFData CreateHapgResponse
