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
-- Module      : Amazonka.CloudHSM.DescribeHapg
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__. For more
-- information, see
-- <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs>,
-- the
-- <https://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide>,
-- and the
-- <https://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference>.
--
-- __For information about the current version of AWS CloudHSM__, see
-- <http://aws.amazon.com/cloudhsm/ AWS CloudHSM>, the
-- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide>,
-- and the
-- <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference>.
--
-- Retrieves information about a high-availability partition group.
module Amazonka.CloudHSM.DescribeHapg
  ( -- * Creating a Request
    DescribeHapg (..),
    newDescribeHapg,

    -- * Request Lenses
    describeHapg_hapgArn,

    -- * Destructuring the Response
    DescribeHapgResponse (..),
    newDescribeHapgResponse,

    -- * Response Lenses
    describeHapgResponse_hapgArn,
    describeHapgResponse_hapgSerial,
    describeHapgResponse_hsmsLastActionFailed,
    describeHapgResponse_hsmsPendingDeletion,
    describeHapgResponse_hsmsPendingRegistration,
    describeHapgResponse_label,
    describeHapgResponse_lastModifiedTimestamp,
    describeHapgResponse_partitionSerialList,
    describeHapgResponse_state,
    describeHapgResponse_httpStatus,
  )
where

import Amazonka.CloudHSM.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the inputs for the DescribeHapg action.
--
-- /See:/ 'newDescribeHapg' smart constructor.
data DescribeHapg = DescribeHapg'
  { -- | The ARN of the high-availability partition group to describe.
    hapgArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeHapg
newDescribeHapg pHapgArn_ =
  DescribeHapg' {hapgArn = pHapgArn_}

-- | The ARN of the high-availability partition group to describe.
describeHapg_hapgArn :: Lens.Lens' DescribeHapg Prelude.Text
describeHapg_hapgArn = Lens.lens (\DescribeHapg' {hapgArn} -> hapgArn) (\s@DescribeHapg' {} a -> s {hapgArn = a} :: DescribeHapg)

instance Core.AWSRequest DescribeHapg where
  type AWSResponse DescribeHapg = DescribeHapgResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeHapgResponse'
            Prelude.<$> (x Data..?> "HapgArn")
            Prelude.<*> (x Data..?> "HapgSerial")
            Prelude.<*> ( x
                            Data..?> "HsmsLastActionFailed"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..?> "HsmsPendingDeletion"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..?> "HsmsPendingRegistration"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "Label")
            Prelude.<*> (x Data..?> "LastModifiedTimestamp")
            Prelude.<*> ( x
                            Data..?> "PartitionSerialList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeHapg where
  hashWithSalt _salt DescribeHapg' {..} =
    _salt `Prelude.hashWithSalt` hapgArn

instance Prelude.NFData DescribeHapg where
  rnf DescribeHapg' {..} = Prelude.rnf hapgArn

instance Data.ToHeaders DescribeHapg where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CloudHsmFrontendService.DescribeHapg" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeHapg where
  toJSON DescribeHapg' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("HapgArn" Data..= hapgArn)]
      )

instance Data.ToPath DescribeHapg where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeHapg where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the output of the DescribeHapg action.
--
-- /See:/ 'newDescribeHapgResponse' smart constructor.
data DescribeHapgResponse = DescribeHapgResponse'
  { -- | The ARN of the high-availability partition group.
    hapgArn :: Prelude.Maybe Prelude.Text,
    -- | The serial number of the high-availability partition group.
    hapgSerial :: Prelude.Maybe Prelude.Text,
    hsmsLastActionFailed :: Prelude.Maybe [Prelude.Text],
    hsmsPendingDeletion :: Prelude.Maybe [Prelude.Text],
    hsmsPendingRegistration :: Prelude.Maybe [Prelude.Text],
    -- | The label for the high-availability partition group.
    label :: Prelude.Maybe Prelude.Text,
    -- | The date and time the high-availability partition group was last
    -- modified.
    lastModifiedTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The list of partition serial numbers that belong to the
    -- high-availability partition group.
    partitionSerialList :: Prelude.Maybe [Prelude.Text],
    -- | The state of the high-availability partition group.
    state :: Prelude.Maybe CloudHsmObjectState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeHapgResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hapgArn', 'describeHapgResponse_hapgArn' - The ARN of the high-availability partition group.
--
-- 'hapgSerial', 'describeHapgResponse_hapgSerial' - The serial number of the high-availability partition group.
--
-- 'hsmsLastActionFailed', 'describeHapgResponse_hsmsLastActionFailed' -
--
-- 'hsmsPendingDeletion', 'describeHapgResponse_hsmsPendingDeletion' -
--
-- 'hsmsPendingRegistration', 'describeHapgResponse_hsmsPendingRegistration' -
--
-- 'label', 'describeHapgResponse_label' - The label for the high-availability partition group.
--
-- 'lastModifiedTimestamp', 'describeHapgResponse_lastModifiedTimestamp' - The date and time the high-availability partition group was last
-- modified.
--
-- 'partitionSerialList', 'describeHapgResponse_partitionSerialList' - The list of partition serial numbers that belong to the
-- high-availability partition group.
--
-- 'state', 'describeHapgResponse_state' - The state of the high-availability partition group.
--
-- 'httpStatus', 'describeHapgResponse_httpStatus' - The response's http status code.
newDescribeHapgResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeHapgResponse
newDescribeHapgResponse pHttpStatus_ =
  DescribeHapgResponse'
    { hapgArn = Prelude.Nothing,
      hapgSerial = Prelude.Nothing,
      hsmsLastActionFailed = Prelude.Nothing,
      hsmsPendingDeletion = Prelude.Nothing,
      hsmsPendingRegistration = Prelude.Nothing,
      label = Prelude.Nothing,
      lastModifiedTimestamp = Prelude.Nothing,
      partitionSerialList = Prelude.Nothing,
      state = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the high-availability partition group.
describeHapgResponse_hapgArn :: Lens.Lens' DescribeHapgResponse (Prelude.Maybe Prelude.Text)
describeHapgResponse_hapgArn = Lens.lens (\DescribeHapgResponse' {hapgArn} -> hapgArn) (\s@DescribeHapgResponse' {} a -> s {hapgArn = a} :: DescribeHapgResponse)

-- | The serial number of the high-availability partition group.
describeHapgResponse_hapgSerial :: Lens.Lens' DescribeHapgResponse (Prelude.Maybe Prelude.Text)
describeHapgResponse_hapgSerial = Lens.lens (\DescribeHapgResponse' {hapgSerial} -> hapgSerial) (\s@DescribeHapgResponse' {} a -> s {hapgSerial = a} :: DescribeHapgResponse)

describeHapgResponse_hsmsLastActionFailed :: Lens.Lens' DescribeHapgResponse (Prelude.Maybe [Prelude.Text])
describeHapgResponse_hsmsLastActionFailed = Lens.lens (\DescribeHapgResponse' {hsmsLastActionFailed} -> hsmsLastActionFailed) (\s@DescribeHapgResponse' {} a -> s {hsmsLastActionFailed = a} :: DescribeHapgResponse) Prelude.. Lens.mapping Lens.coerced

describeHapgResponse_hsmsPendingDeletion :: Lens.Lens' DescribeHapgResponse (Prelude.Maybe [Prelude.Text])
describeHapgResponse_hsmsPendingDeletion = Lens.lens (\DescribeHapgResponse' {hsmsPendingDeletion} -> hsmsPendingDeletion) (\s@DescribeHapgResponse' {} a -> s {hsmsPendingDeletion = a} :: DescribeHapgResponse) Prelude.. Lens.mapping Lens.coerced

describeHapgResponse_hsmsPendingRegistration :: Lens.Lens' DescribeHapgResponse (Prelude.Maybe [Prelude.Text])
describeHapgResponse_hsmsPendingRegistration = Lens.lens (\DescribeHapgResponse' {hsmsPendingRegistration} -> hsmsPendingRegistration) (\s@DescribeHapgResponse' {} a -> s {hsmsPendingRegistration = a} :: DescribeHapgResponse) Prelude.. Lens.mapping Lens.coerced

-- | The label for the high-availability partition group.
describeHapgResponse_label :: Lens.Lens' DescribeHapgResponse (Prelude.Maybe Prelude.Text)
describeHapgResponse_label = Lens.lens (\DescribeHapgResponse' {label} -> label) (\s@DescribeHapgResponse' {} a -> s {label = a} :: DescribeHapgResponse)

-- | The date and time the high-availability partition group was last
-- modified.
describeHapgResponse_lastModifiedTimestamp :: Lens.Lens' DescribeHapgResponse (Prelude.Maybe Prelude.Text)
describeHapgResponse_lastModifiedTimestamp = Lens.lens (\DescribeHapgResponse' {lastModifiedTimestamp} -> lastModifiedTimestamp) (\s@DescribeHapgResponse' {} a -> s {lastModifiedTimestamp = a} :: DescribeHapgResponse)

-- | The list of partition serial numbers that belong to the
-- high-availability partition group.
describeHapgResponse_partitionSerialList :: Lens.Lens' DescribeHapgResponse (Prelude.Maybe [Prelude.Text])
describeHapgResponse_partitionSerialList = Lens.lens (\DescribeHapgResponse' {partitionSerialList} -> partitionSerialList) (\s@DescribeHapgResponse' {} a -> s {partitionSerialList = a} :: DescribeHapgResponse) Prelude.. Lens.mapping Lens.coerced

-- | The state of the high-availability partition group.
describeHapgResponse_state :: Lens.Lens' DescribeHapgResponse (Prelude.Maybe CloudHsmObjectState)
describeHapgResponse_state = Lens.lens (\DescribeHapgResponse' {state} -> state) (\s@DescribeHapgResponse' {} a -> s {state = a} :: DescribeHapgResponse)

-- | The response's http status code.
describeHapgResponse_httpStatus :: Lens.Lens' DescribeHapgResponse Prelude.Int
describeHapgResponse_httpStatus = Lens.lens (\DescribeHapgResponse' {httpStatus} -> httpStatus) (\s@DescribeHapgResponse' {} a -> s {httpStatus = a} :: DescribeHapgResponse)

instance Prelude.NFData DescribeHapgResponse where
  rnf DescribeHapgResponse' {..} =
    Prelude.rnf hapgArn `Prelude.seq`
      Prelude.rnf hapgSerial `Prelude.seq`
        Prelude.rnf hsmsLastActionFailed `Prelude.seq`
          Prelude.rnf hsmsPendingDeletion `Prelude.seq`
            Prelude.rnf hsmsPendingRegistration `Prelude.seq`
              Prelude.rnf label `Prelude.seq`
                Prelude.rnf lastModifiedTimestamp `Prelude.seq`
                  Prelude.rnf partitionSerialList `Prelude.seq`
                    Prelude.rnf state `Prelude.seq`
                      Prelude.rnf httpStatus
