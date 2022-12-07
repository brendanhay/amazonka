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
-- Module      : Amazonka.CloudHSM.ModifyHapg
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
-- Modifies an existing high-availability partition group.
module Amazonka.CloudHSM.ModifyHapg
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

import Amazonka.CloudHSM.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyHapg' smart constructor.
data ModifyHapg = ModifyHapg'
  { -- | The list of partition serial numbers to make members of the
    -- high-availability partition group.
    partitionSerialList :: Prelude.Maybe [Prelude.Text],
    -- | The new label for the high-availability partition group.
    label :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the high-availability partition group to modify.
    hapgArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ModifyHapg
newModifyHapg pHapgArn_ =
  ModifyHapg'
    { partitionSerialList = Prelude.Nothing,
      label = Prelude.Nothing,
      hapgArn = pHapgArn_
    }

-- | The list of partition serial numbers to make members of the
-- high-availability partition group.
modifyHapg_partitionSerialList :: Lens.Lens' ModifyHapg (Prelude.Maybe [Prelude.Text])
modifyHapg_partitionSerialList = Lens.lens (\ModifyHapg' {partitionSerialList} -> partitionSerialList) (\s@ModifyHapg' {} a -> s {partitionSerialList = a} :: ModifyHapg) Prelude.. Lens.mapping Lens.coerced

-- | The new label for the high-availability partition group.
modifyHapg_label :: Lens.Lens' ModifyHapg (Prelude.Maybe Prelude.Text)
modifyHapg_label = Lens.lens (\ModifyHapg' {label} -> label) (\s@ModifyHapg' {} a -> s {label = a} :: ModifyHapg)

-- | The ARN of the high-availability partition group to modify.
modifyHapg_hapgArn :: Lens.Lens' ModifyHapg Prelude.Text
modifyHapg_hapgArn = Lens.lens (\ModifyHapg' {hapgArn} -> hapgArn) (\s@ModifyHapg' {} a -> s {hapgArn = a} :: ModifyHapg)

instance Core.AWSRequest ModifyHapg where
  type AWSResponse ModifyHapg = ModifyHapgResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ModifyHapgResponse'
            Prelude.<$> (x Data..?> "HapgArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyHapg where
  hashWithSalt _salt ModifyHapg' {..} =
    _salt `Prelude.hashWithSalt` partitionSerialList
      `Prelude.hashWithSalt` label
      `Prelude.hashWithSalt` hapgArn

instance Prelude.NFData ModifyHapg where
  rnf ModifyHapg' {..} =
    Prelude.rnf partitionSerialList
      `Prelude.seq` Prelude.rnf label
      `Prelude.seq` Prelude.rnf hapgArn

instance Data.ToHeaders ModifyHapg where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CloudHsmFrontendService.ModifyHapg" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ModifyHapg where
  toJSON ModifyHapg' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PartitionSerialList" Data..=)
              Prelude.<$> partitionSerialList,
            ("Label" Data..=) Prelude.<$> label,
            Prelude.Just ("HapgArn" Data..= hapgArn)
          ]
      )

instance Data.ToPath ModifyHapg where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyHapg where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newModifyHapgResponse' smart constructor.
data ModifyHapgResponse = ModifyHapgResponse'
  { -- | The ARN of the high-availability partition group.
    hapgArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ModifyHapgResponse
newModifyHapgResponse pHttpStatus_ =
  ModifyHapgResponse'
    { hapgArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the high-availability partition group.
modifyHapgResponse_hapgArn :: Lens.Lens' ModifyHapgResponse (Prelude.Maybe Prelude.Text)
modifyHapgResponse_hapgArn = Lens.lens (\ModifyHapgResponse' {hapgArn} -> hapgArn) (\s@ModifyHapgResponse' {} a -> s {hapgArn = a} :: ModifyHapgResponse)

-- | The response's http status code.
modifyHapgResponse_httpStatus :: Lens.Lens' ModifyHapgResponse Prelude.Int
modifyHapgResponse_httpStatus = Lens.lens (\ModifyHapgResponse' {httpStatus} -> httpStatus) (\s@ModifyHapgResponse' {} a -> s {httpStatus = a} :: ModifyHapgResponse)

instance Prelude.NFData ModifyHapgResponse where
  rnf ModifyHapgResponse' {..} =
    Prelude.rnf hapgArn
      `Prelude.seq` Prelude.rnf httpStatus
