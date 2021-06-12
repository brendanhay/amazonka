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
-- Module      : Network.AWS.CloudHSM.DescribeHapg
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
-- Retrieves information about a high-availability partition group.
module Network.AWS.CloudHSM.DescribeHapg
  ( -- * Creating a Request
    DescribeHapg (..),
    newDescribeHapg,

    -- * Request Lenses
    describeHapg_hapgArn,

    -- * Destructuring the Response
    DescribeHapgResponse (..),
    newDescribeHapgResponse,

    -- * Response Lenses
    describeHapgResponse_hsmsPendingDeletion,
    describeHapgResponse_hapgArn,
    describeHapgResponse_partitionSerialList,
    describeHapgResponse_lastModifiedTimestamp,
    describeHapgResponse_state,
    describeHapgResponse_label,
    describeHapgResponse_hapgSerial,
    describeHapgResponse_hsmsPendingRegistration,
    describeHapgResponse_hsmsLastActionFailed,
    describeHapgResponse_httpStatus,
  )
where

import Network.AWS.CloudHSM.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the DescribeHapg action.
--
-- /See:/ 'newDescribeHapg' smart constructor.
data DescribeHapg = DescribeHapg'
  { -- | The ARN of the high-availability partition group to describe.
    hapgArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeHapg' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hapgArn', 'describeHapg_hapgArn' - The ARN of the high-availability partition group to describe.
newDescribeHapg ::
  -- | 'hapgArn'
  Core.Text ->
  DescribeHapg
newDescribeHapg pHapgArn_ =
  DescribeHapg' {hapgArn = pHapgArn_}

-- | The ARN of the high-availability partition group to describe.
describeHapg_hapgArn :: Lens.Lens' DescribeHapg Core.Text
describeHapg_hapgArn = Lens.lens (\DescribeHapg' {hapgArn} -> hapgArn) (\s@DescribeHapg' {} a -> s {hapgArn = a} :: DescribeHapg)

instance Core.AWSRequest DescribeHapg where
  type AWSResponse DescribeHapg = DescribeHapgResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeHapgResponse'
            Core.<$> ( x Core..?> "HsmsPendingDeletion"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "HapgArn")
            Core.<*> ( x Core..?> "PartitionSerialList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "LastModifiedTimestamp")
            Core.<*> (x Core..?> "State")
            Core.<*> (x Core..?> "Label")
            Core.<*> (x Core..?> "HapgSerial")
            Core.<*> ( x Core..?> "HsmsPendingRegistration"
                         Core..!@ Core.mempty
                     )
            Core.<*> ( x Core..?> "HsmsLastActionFailed"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeHapg

instance Core.NFData DescribeHapg

instance Core.ToHeaders DescribeHapg where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CloudHsmFrontendService.DescribeHapg" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeHapg where
  toJSON DescribeHapg' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("HapgArn" Core..= hapgArn)]
      )

instance Core.ToPath DescribeHapg where
  toPath = Core.const "/"

instance Core.ToQuery DescribeHapg where
  toQuery = Core.const Core.mempty

-- | Contains the output of the DescribeHapg action.
--
-- /See:/ 'newDescribeHapgResponse' smart constructor.
data DescribeHapgResponse = DescribeHapgResponse'
  { hsmsPendingDeletion :: Core.Maybe [Core.Text],
    -- | The ARN of the high-availability partition group.
    hapgArn :: Core.Maybe Core.Text,
    -- | The list of partition serial numbers that belong to the
    -- high-availability partition group.
    partitionSerialList :: Core.Maybe [Core.Text],
    -- | The date and time the high-availability partition group was last
    -- modified.
    lastModifiedTimestamp :: Core.Maybe Core.Text,
    -- | The state of the high-availability partition group.
    state :: Core.Maybe CloudHsmObjectState,
    -- | The label for the high-availability partition group.
    label :: Core.Maybe Core.Text,
    -- | The serial number of the high-availability partition group.
    hapgSerial :: Core.Maybe Core.Text,
    hsmsPendingRegistration :: Core.Maybe [Core.Text],
    hsmsLastActionFailed :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeHapgResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hsmsPendingDeletion', 'describeHapgResponse_hsmsPendingDeletion' -
--
-- 'hapgArn', 'describeHapgResponse_hapgArn' - The ARN of the high-availability partition group.
--
-- 'partitionSerialList', 'describeHapgResponse_partitionSerialList' - The list of partition serial numbers that belong to the
-- high-availability partition group.
--
-- 'lastModifiedTimestamp', 'describeHapgResponse_lastModifiedTimestamp' - The date and time the high-availability partition group was last
-- modified.
--
-- 'state', 'describeHapgResponse_state' - The state of the high-availability partition group.
--
-- 'label', 'describeHapgResponse_label' - The label for the high-availability partition group.
--
-- 'hapgSerial', 'describeHapgResponse_hapgSerial' - The serial number of the high-availability partition group.
--
-- 'hsmsPendingRegistration', 'describeHapgResponse_hsmsPendingRegistration' -
--
-- 'hsmsLastActionFailed', 'describeHapgResponse_hsmsLastActionFailed' -
--
-- 'httpStatus', 'describeHapgResponse_httpStatus' - The response's http status code.
newDescribeHapgResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeHapgResponse
newDescribeHapgResponse pHttpStatus_ =
  DescribeHapgResponse'
    { hsmsPendingDeletion =
        Core.Nothing,
      hapgArn = Core.Nothing,
      partitionSerialList = Core.Nothing,
      lastModifiedTimestamp = Core.Nothing,
      state = Core.Nothing,
      label = Core.Nothing,
      hapgSerial = Core.Nothing,
      hsmsPendingRegistration = Core.Nothing,
      hsmsLastActionFailed = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- |
describeHapgResponse_hsmsPendingDeletion :: Lens.Lens' DescribeHapgResponse (Core.Maybe [Core.Text])
describeHapgResponse_hsmsPendingDeletion = Lens.lens (\DescribeHapgResponse' {hsmsPendingDeletion} -> hsmsPendingDeletion) (\s@DescribeHapgResponse' {} a -> s {hsmsPendingDeletion = a} :: DescribeHapgResponse) Core.. Lens.mapping Lens._Coerce

-- | The ARN of the high-availability partition group.
describeHapgResponse_hapgArn :: Lens.Lens' DescribeHapgResponse (Core.Maybe Core.Text)
describeHapgResponse_hapgArn = Lens.lens (\DescribeHapgResponse' {hapgArn} -> hapgArn) (\s@DescribeHapgResponse' {} a -> s {hapgArn = a} :: DescribeHapgResponse)

-- | The list of partition serial numbers that belong to the
-- high-availability partition group.
describeHapgResponse_partitionSerialList :: Lens.Lens' DescribeHapgResponse (Core.Maybe [Core.Text])
describeHapgResponse_partitionSerialList = Lens.lens (\DescribeHapgResponse' {partitionSerialList} -> partitionSerialList) (\s@DescribeHapgResponse' {} a -> s {partitionSerialList = a} :: DescribeHapgResponse) Core.. Lens.mapping Lens._Coerce

-- | The date and time the high-availability partition group was last
-- modified.
describeHapgResponse_lastModifiedTimestamp :: Lens.Lens' DescribeHapgResponse (Core.Maybe Core.Text)
describeHapgResponse_lastModifiedTimestamp = Lens.lens (\DescribeHapgResponse' {lastModifiedTimestamp} -> lastModifiedTimestamp) (\s@DescribeHapgResponse' {} a -> s {lastModifiedTimestamp = a} :: DescribeHapgResponse)

-- | The state of the high-availability partition group.
describeHapgResponse_state :: Lens.Lens' DescribeHapgResponse (Core.Maybe CloudHsmObjectState)
describeHapgResponse_state = Lens.lens (\DescribeHapgResponse' {state} -> state) (\s@DescribeHapgResponse' {} a -> s {state = a} :: DescribeHapgResponse)

-- | The label for the high-availability partition group.
describeHapgResponse_label :: Lens.Lens' DescribeHapgResponse (Core.Maybe Core.Text)
describeHapgResponse_label = Lens.lens (\DescribeHapgResponse' {label} -> label) (\s@DescribeHapgResponse' {} a -> s {label = a} :: DescribeHapgResponse)

-- | The serial number of the high-availability partition group.
describeHapgResponse_hapgSerial :: Lens.Lens' DescribeHapgResponse (Core.Maybe Core.Text)
describeHapgResponse_hapgSerial = Lens.lens (\DescribeHapgResponse' {hapgSerial} -> hapgSerial) (\s@DescribeHapgResponse' {} a -> s {hapgSerial = a} :: DescribeHapgResponse)

-- |
describeHapgResponse_hsmsPendingRegistration :: Lens.Lens' DescribeHapgResponse (Core.Maybe [Core.Text])
describeHapgResponse_hsmsPendingRegistration = Lens.lens (\DescribeHapgResponse' {hsmsPendingRegistration} -> hsmsPendingRegistration) (\s@DescribeHapgResponse' {} a -> s {hsmsPendingRegistration = a} :: DescribeHapgResponse) Core.. Lens.mapping Lens._Coerce

-- |
describeHapgResponse_hsmsLastActionFailed :: Lens.Lens' DescribeHapgResponse (Core.Maybe [Core.Text])
describeHapgResponse_hsmsLastActionFailed = Lens.lens (\DescribeHapgResponse' {hsmsLastActionFailed} -> hsmsLastActionFailed) (\s@DescribeHapgResponse' {} a -> s {hsmsLastActionFailed = a} :: DescribeHapgResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeHapgResponse_httpStatus :: Lens.Lens' DescribeHapgResponse Core.Int
describeHapgResponse_httpStatus = Lens.lens (\DescribeHapgResponse' {httpStatus} -> httpStatus) (\s@DescribeHapgResponse' {} a -> s {httpStatus = a} :: DescribeHapgResponse)

instance Core.NFData DescribeHapgResponse
