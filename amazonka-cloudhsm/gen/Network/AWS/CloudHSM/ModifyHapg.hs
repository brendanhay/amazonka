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
-- Module      : Network.AWS.CloudHSM.ModifyHapg
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
-- Modifies an existing high-availability partition group.
module Network.AWS.CloudHSM.ModifyHapg
  ( -- * Creating a Request
    ModifyHapg (..),
    newModifyHapg,

    -- * Request Lenses
    modifyHapg_partitionSerialList,
    modifyHapg_label,
    modifyHapg_hapgArn,

    -- * Destructuring the Response
    ModifyHapgResponse (..),
    newModifyHapgResponse,

    -- * Response Lenses
    modifyHapgResponse_hapgArn,
    modifyHapgResponse_httpStatus,
  )
where

import Network.AWS.CloudHSM.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyHapg' smart constructor.
data ModifyHapg = ModifyHapg'
  { -- | The list of partition serial numbers to make members of the
    -- high-availability partition group.
    partitionSerialList :: Core.Maybe [Core.Text],
    -- | The new label for the high-availability partition group.
    label :: Core.Maybe Core.Text,
    -- | The ARN of the high-availability partition group to modify.
    hapgArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyHapg' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'partitionSerialList', 'modifyHapg_partitionSerialList' - The list of partition serial numbers to make members of the
-- high-availability partition group.
--
-- 'label', 'modifyHapg_label' - The new label for the high-availability partition group.
--
-- 'hapgArn', 'modifyHapg_hapgArn' - The ARN of the high-availability partition group to modify.
newModifyHapg ::
  -- | 'hapgArn'
  Core.Text ->
  ModifyHapg
newModifyHapg pHapgArn_ =
  ModifyHapg'
    { partitionSerialList = Core.Nothing,
      label = Core.Nothing,
      hapgArn = pHapgArn_
    }

-- | The list of partition serial numbers to make members of the
-- high-availability partition group.
modifyHapg_partitionSerialList :: Lens.Lens' ModifyHapg (Core.Maybe [Core.Text])
modifyHapg_partitionSerialList = Lens.lens (\ModifyHapg' {partitionSerialList} -> partitionSerialList) (\s@ModifyHapg' {} a -> s {partitionSerialList = a} :: ModifyHapg) Core.. Lens.mapping Lens._Coerce

-- | The new label for the high-availability partition group.
modifyHapg_label :: Lens.Lens' ModifyHapg (Core.Maybe Core.Text)
modifyHapg_label = Lens.lens (\ModifyHapg' {label} -> label) (\s@ModifyHapg' {} a -> s {label = a} :: ModifyHapg)

-- | The ARN of the high-availability partition group to modify.
modifyHapg_hapgArn :: Lens.Lens' ModifyHapg Core.Text
modifyHapg_hapgArn = Lens.lens (\ModifyHapg' {hapgArn} -> hapgArn) (\s@ModifyHapg' {} a -> s {hapgArn = a} :: ModifyHapg)

instance Core.AWSRequest ModifyHapg where
  type AWSResponse ModifyHapg = ModifyHapgResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ModifyHapgResponse'
            Core.<$> (x Core..?> "HapgArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyHapg

instance Core.NFData ModifyHapg

instance Core.ToHeaders ModifyHapg where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CloudHsmFrontendService.ModifyHapg" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ModifyHapg where
  toJSON ModifyHapg' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PartitionSerialList" Core..=)
              Core.<$> partitionSerialList,
            ("Label" Core..=) Core.<$> label,
            Core.Just ("HapgArn" Core..= hapgArn)
          ]
      )

instance Core.ToPath ModifyHapg where
  toPath = Core.const "/"

instance Core.ToQuery ModifyHapg where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newModifyHapgResponse' smart constructor.
data ModifyHapgResponse = ModifyHapgResponse'
  { -- | The ARN of the high-availability partition group.
    hapgArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyHapgResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hapgArn', 'modifyHapgResponse_hapgArn' - The ARN of the high-availability partition group.
--
-- 'httpStatus', 'modifyHapgResponse_httpStatus' - The response's http status code.
newModifyHapgResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifyHapgResponse
newModifyHapgResponse pHttpStatus_ =
  ModifyHapgResponse'
    { hapgArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the high-availability partition group.
modifyHapgResponse_hapgArn :: Lens.Lens' ModifyHapgResponse (Core.Maybe Core.Text)
modifyHapgResponse_hapgArn = Lens.lens (\ModifyHapgResponse' {hapgArn} -> hapgArn) (\s@ModifyHapgResponse' {} a -> s {hapgArn = a} :: ModifyHapgResponse)

-- | The response's http status code.
modifyHapgResponse_httpStatus :: Lens.Lens' ModifyHapgResponse Core.Int
modifyHapgResponse_httpStatus = Lens.lens (\ModifyHapgResponse' {httpStatus} -> httpStatus) (\s@ModifyHapgResponse' {} a -> s {httpStatus = a} :: ModifyHapgResponse)

instance Core.NFData ModifyHapgResponse
