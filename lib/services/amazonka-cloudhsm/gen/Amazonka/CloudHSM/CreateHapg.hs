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
-- Module      : Amazonka.CloudHSM.CreateHapg
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- Creates a high-availability partition group. A high-availability
-- partition group is a group of partitions that spans multiple physical
-- HSMs.
module Amazonka.CloudHSM.CreateHapg
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

import Amazonka.CloudHSM.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the inputs for the CreateHapgRequest action.
--
-- /See:/ 'newCreateHapg' smart constructor.
data CreateHapg = CreateHapg'
  { -- | The label of the new high-availability partition group.
    label :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  CreateHapg
newCreateHapg pLabel_ = CreateHapg' {label = pLabel_}

-- | The label of the new high-availability partition group.
createHapg_label :: Lens.Lens' CreateHapg Prelude.Text
createHapg_label = Lens.lens (\CreateHapg' {label} -> label) (\s@CreateHapg' {} a -> s {label = a} :: CreateHapg)

instance Core.AWSRequest CreateHapg where
  type AWSResponse CreateHapg = CreateHapgResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateHapgResponse'
            Prelude.<$> (x Data..?> "HapgArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateHapg where
  hashWithSalt _salt CreateHapg' {..} =
    _salt `Prelude.hashWithSalt` label

instance Prelude.NFData CreateHapg where
  rnf CreateHapg' {..} = Prelude.rnf label

instance Data.ToHeaders CreateHapg where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CloudHsmFrontendService.CreateHapg" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateHapg where
  toJSON CreateHapg' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Label" Data..= label)]
      )

instance Data.ToPath CreateHapg where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateHapg where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the output of the CreateHAPartitionGroup action.
--
-- /See:/ 'newCreateHapgResponse' smart constructor.
data CreateHapgResponse = CreateHapgResponse'
  { -- | The ARN of the high-availability partition group.
    hapgArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateHapgResponse
newCreateHapgResponse pHttpStatus_ =
  CreateHapgResponse'
    { hapgArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the high-availability partition group.
createHapgResponse_hapgArn :: Lens.Lens' CreateHapgResponse (Prelude.Maybe Prelude.Text)
createHapgResponse_hapgArn = Lens.lens (\CreateHapgResponse' {hapgArn} -> hapgArn) (\s@CreateHapgResponse' {} a -> s {hapgArn = a} :: CreateHapgResponse)

-- | The response's http status code.
createHapgResponse_httpStatus :: Lens.Lens' CreateHapgResponse Prelude.Int
createHapgResponse_httpStatus = Lens.lens (\CreateHapgResponse' {httpStatus} -> httpStatus) (\s@CreateHapgResponse' {} a -> s {httpStatus = a} :: CreateHapgResponse)

instance Prelude.NFData CreateHapgResponse where
  rnf CreateHapgResponse' {..} =
    Prelude.rnf hapgArn
      `Prelude.seq` Prelude.rnf httpStatus
