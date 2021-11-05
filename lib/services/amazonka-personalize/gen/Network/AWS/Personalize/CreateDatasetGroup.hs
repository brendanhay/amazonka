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
-- Module      : Amazonka.Personalize.CreateDatasetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an empty dataset group. A dataset group contains related
-- datasets that supply data for training a model. A dataset group can
-- contain at most three datasets, one for each type of dataset:
--
-- -   Interactions
--
-- -   Items
--
-- -   Users
--
-- To train a model (create a solution), a dataset group that contains an
-- @Interactions@ dataset is required. Call CreateDataset to add a dataset
-- to the group.
--
-- A dataset group can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   DELETE PENDING
--
-- To get the status of the dataset group, call DescribeDatasetGroup. If
-- the status shows as CREATE FAILED, the response includes a
-- @failureReason@ key, which describes why the creation failed.
--
-- You must wait until the @status@ of the dataset group is @ACTIVE@ before
-- adding a dataset to the group.
--
-- You can specify an Key Management Service (KMS) key to encrypt the
-- datasets in the group. If you specify a KMS key, you must also include
-- an Identity and Access Management (IAM) role that has permission to
-- access the key.
--
-- __APIs that require a dataset group ARN in the request__
--
-- -   CreateDataset
--
-- -   CreateEventTracker
--
-- -   CreateSolution
--
-- __Related APIs__
--
-- -   ListDatasetGroups
--
-- -   DescribeDatasetGroup
--
-- -   DeleteDatasetGroup
module Amazonka.Personalize.CreateDatasetGroup
  ( -- * Creating a Request
    CreateDatasetGroup (..),
    newCreateDatasetGroup,

    -- * Request Lenses
    createDatasetGroup_kmsKeyArn,
    createDatasetGroup_roleArn,
    createDatasetGroup_name,

    -- * Destructuring the Response
    CreateDatasetGroupResponse (..),
    newCreateDatasetGroupResponse,

    -- * Response Lenses
    createDatasetGroupResponse_datasetGroupArn,
    createDatasetGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDatasetGroup' smart constructor.
data CreateDatasetGroup = CreateDatasetGroup'
  { -- | The Amazon Resource Name (ARN) of a Key Management Service (KMS) key
    -- used to encrypt the datasets.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Identity and Access Management (IAM) role that has
    -- permissions to access the Key Management Service (KMS) key. Supplying an
    -- IAM role is only valid when also specifying a KMS key.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The name for the new dataset group.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDatasetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyArn', 'createDatasetGroup_kmsKeyArn' - The Amazon Resource Name (ARN) of a Key Management Service (KMS) key
-- used to encrypt the datasets.
--
-- 'roleArn', 'createDatasetGroup_roleArn' - The ARN of the Identity and Access Management (IAM) role that has
-- permissions to access the Key Management Service (KMS) key. Supplying an
-- IAM role is only valid when also specifying a KMS key.
--
-- 'name', 'createDatasetGroup_name' - The name for the new dataset group.
newCreateDatasetGroup ::
  -- | 'name'
  Prelude.Text ->
  CreateDatasetGroup
newCreateDatasetGroup pName_ =
  CreateDatasetGroup'
    { kmsKeyArn = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      name = pName_
    }

-- | The Amazon Resource Name (ARN) of a Key Management Service (KMS) key
-- used to encrypt the datasets.
createDatasetGroup_kmsKeyArn :: Lens.Lens' CreateDatasetGroup (Prelude.Maybe Prelude.Text)
createDatasetGroup_kmsKeyArn = Lens.lens (\CreateDatasetGroup' {kmsKeyArn} -> kmsKeyArn) (\s@CreateDatasetGroup' {} a -> s {kmsKeyArn = a} :: CreateDatasetGroup)

-- | The ARN of the Identity and Access Management (IAM) role that has
-- permissions to access the Key Management Service (KMS) key. Supplying an
-- IAM role is only valid when also specifying a KMS key.
createDatasetGroup_roleArn :: Lens.Lens' CreateDatasetGroup (Prelude.Maybe Prelude.Text)
createDatasetGroup_roleArn = Lens.lens (\CreateDatasetGroup' {roleArn} -> roleArn) (\s@CreateDatasetGroup' {} a -> s {roleArn = a} :: CreateDatasetGroup)

-- | The name for the new dataset group.
createDatasetGroup_name :: Lens.Lens' CreateDatasetGroup Prelude.Text
createDatasetGroup_name = Lens.lens (\CreateDatasetGroup' {name} -> name) (\s@CreateDatasetGroup' {} a -> s {name = a} :: CreateDatasetGroup)

instance Core.AWSRequest CreateDatasetGroup where
  type
    AWSResponse CreateDatasetGroup =
      CreateDatasetGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDatasetGroupResponse'
            Prelude.<$> (x Core..?> "datasetGroupArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDatasetGroup

instance Prelude.NFData CreateDatasetGroup

instance Core.ToHeaders CreateDatasetGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonPersonalize.CreateDatasetGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateDatasetGroup where
  toJSON CreateDatasetGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("kmsKeyArn" Core..=) Prelude.<$> kmsKeyArn,
            ("roleArn" Core..=) Prelude.<$> roleArn,
            Prelude.Just ("name" Core..= name)
          ]
      )

instance Core.ToPath CreateDatasetGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateDatasetGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDatasetGroupResponse' smart constructor.
data CreateDatasetGroupResponse = CreateDatasetGroupResponse'
  { -- | The Amazon Resource Name (ARN) of the new dataset group.
    datasetGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDatasetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetGroupArn', 'createDatasetGroupResponse_datasetGroupArn' - The Amazon Resource Name (ARN) of the new dataset group.
--
-- 'httpStatus', 'createDatasetGroupResponse_httpStatus' - The response's http status code.
newCreateDatasetGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDatasetGroupResponse
newCreateDatasetGroupResponse pHttpStatus_ =
  CreateDatasetGroupResponse'
    { datasetGroupArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the new dataset group.
createDatasetGroupResponse_datasetGroupArn :: Lens.Lens' CreateDatasetGroupResponse (Prelude.Maybe Prelude.Text)
createDatasetGroupResponse_datasetGroupArn = Lens.lens (\CreateDatasetGroupResponse' {datasetGroupArn} -> datasetGroupArn) (\s@CreateDatasetGroupResponse' {} a -> s {datasetGroupArn = a} :: CreateDatasetGroupResponse)

-- | The response's http status code.
createDatasetGroupResponse_httpStatus :: Lens.Lens' CreateDatasetGroupResponse Prelude.Int
createDatasetGroupResponse_httpStatus = Lens.lens (\CreateDatasetGroupResponse' {httpStatus} -> httpStatus) (\s@CreateDatasetGroupResponse' {} a -> s {httpStatus = a} :: CreateDatasetGroupResponse)

instance Prelude.NFData CreateDatasetGroupResponse
