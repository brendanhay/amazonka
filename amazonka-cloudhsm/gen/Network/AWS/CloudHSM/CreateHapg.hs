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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the CreateHapgRequest action.
--
-- /See:/ 'newCreateHapg' smart constructor.
data CreateHapg = CreateHapg'
  { -- | The label of the new high-availability partition group.
    label :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest CreateHapg where
  type Rs CreateHapg = CreateHapgResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateHapgResponse'
            Prelude.<$> (x Prelude..?> "HapgArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateHapg

instance Prelude.NFData CreateHapg

instance Prelude.ToHeaders CreateHapg where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CloudHsmFrontendService.CreateHapg" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateHapg where
  toJSON CreateHapg' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Label" Prelude..= label)]
      )

instance Prelude.ToPath CreateHapg where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateHapg where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData CreateHapgResponse
