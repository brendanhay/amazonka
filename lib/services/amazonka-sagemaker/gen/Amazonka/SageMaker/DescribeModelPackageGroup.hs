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
-- Module      : Amazonka.SageMaker.DescribeModelPackageGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a description for the specified model group.
module Amazonka.SageMaker.DescribeModelPackageGroup
  ( -- * Creating a Request
    DescribeModelPackageGroup (..),
    newDescribeModelPackageGroup,

    -- * Request Lenses
    describeModelPackageGroup_modelPackageGroupName,

    -- * Destructuring the Response
    DescribeModelPackageGroupResponse (..),
    newDescribeModelPackageGroupResponse,

    -- * Response Lenses
    describeModelPackageGroupResponse_modelPackageGroupDescription,
    describeModelPackageGroupResponse_httpStatus,
    describeModelPackageGroupResponse_modelPackageGroupName,
    describeModelPackageGroupResponse_modelPackageGroupArn,
    describeModelPackageGroupResponse_creationTime,
    describeModelPackageGroupResponse_createdBy,
    describeModelPackageGroupResponse_modelPackageGroupStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeModelPackageGroup' smart constructor.
data DescribeModelPackageGroup = DescribeModelPackageGroup'
  { -- | The name of gthe model group to describe.
    modelPackageGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeModelPackageGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelPackageGroupName', 'describeModelPackageGroup_modelPackageGroupName' - The name of gthe model group to describe.
newDescribeModelPackageGroup ::
  -- | 'modelPackageGroupName'
  Prelude.Text ->
  DescribeModelPackageGroup
newDescribeModelPackageGroup pModelPackageGroupName_ =
  DescribeModelPackageGroup'
    { modelPackageGroupName =
        pModelPackageGroupName_
    }

-- | The name of gthe model group to describe.
describeModelPackageGroup_modelPackageGroupName :: Lens.Lens' DescribeModelPackageGroup Prelude.Text
describeModelPackageGroup_modelPackageGroupName = Lens.lens (\DescribeModelPackageGroup' {modelPackageGroupName} -> modelPackageGroupName) (\s@DescribeModelPackageGroup' {} a -> s {modelPackageGroupName = a} :: DescribeModelPackageGroup)

instance Core.AWSRequest DescribeModelPackageGroup where
  type
    AWSResponse DescribeModelPackageGroup =
      DescribeModelPackageGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeModelPackageGroupResponse'
            Prelude.<$> (x Data..?> "ModelPackageGroupDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ModelPackageGroupName")
            Prelude.<*> (x Data..:> "ModelPackageGroupArn")
            Prelude.<*> (x Data..:> "CreationTime")
            Prelude.<*> (x Data..:> "CreatedBy")
            Prelude.<*> (x Data..:> "ModelPackageGroupStatus")
      )

instance Prelude.Hashable DescribeModelPackageGroup where
  hashWithSalt _salt DescribeModelPackageGroup' {..} =
    _salt `Prelude.hashWithSalt` modelPackageGroupName

instance Prelude.NFData DescribeModelPackageGroup where
  rnf DescribeModelPackageGroup' {..} =
    Prelude.rnf modelPackageGroupName

instance Data.ToHeaders DescribeModelPackageGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DescribeModelPackageGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeModelPackageGroup where
  toJSON DescribeModelPackageGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ModelPackageGroupName"
                  Data..= modelPackageGroupName
              )
          ]
      )

instance Data.ToPath DescribeModelPackageGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeModelPackageGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeModelPackageGroupResponse' smart constructor.
data DescribeModelPackageGroupResponse = DescribeModelPackageGroupResponse'
  { -- | A description of the model group.
    modelPackageGroupDescription :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the model group.
    modelPackageGroupName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the model group.
    modelPackageGroupArn :: Prelude.Text,
    -- | The time that the model group was created.
    creationTime :: Data.POSIX,
    createdBy :: UserContext,
    -- | The status of the model group.
    modelPackageGroupStatus :: ModelPackageGroupStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeModelPackageGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelPackageGroupDescription', 'describeModelPackageGroupResponse_modelPackageGroupDescription' - A description of the model group.
--
-- 'httpStatus', 'describeModelPackageGroupResponse_httpStatus' - The response's http status code.
--
-- 'modelPackageGroupName', 'describeModelPackageGroupResponse_modelPackageGroupName' - The name of the model group.
--
-- 'modelPackageGroupArn', 'describeModelPackageGroupResponse_modelPackageGroupArn' - The Amazon Resource Name (ARN) of the model group.
--
-- 'creationTime', 'describeModelPackageGroupResponse_creationTime' - The time that the model group was created.
--
-- 'createdBy', 'describeModelPackageGroupResponse_createdBy' - Undocumented member.
--
-- 'modelPackageGroupStatus', 'describeModelPackageGroupResponse_modelPackageGroupStatus' - The status of the model group.
newDescribeModelPackageGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'modelPackageGroupName'
  Prelude.Text ->
  -- | 'modelPackageGroupArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'createdBy'
  UserContext ->
  -- | 'modelPackageGroupStatus'
  ModelPackageGroupStatus ->
  DescribeModelPackageGroupResponse
newDescribeModelPackageGroupResponse
  pHttpStatus_
  pModelPackageGroupName_
  pModelPackageGroupArn_
  pCreationTime_
  pCreatedBy_
  pModelPackageGroupStatus_ =
    DescribeModelPackageGroupResponse'
      { modelPackageGroupDescription =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        modelPackageGroupName =
          pModelPackageGroupName_,
        modelPackageGroupArn =
          pModelPackageGroupArn_,
        creationTime =
          Data._Time Lens.# pCreationTime_,
        createdBy = pCreatedBy_,
        modelPackageGroupStatus =
          pModelPackageGroupStatus_
      }

-- | A description of the model group.
describeModelPackageGroupResponse_modelPackageGroupDescription :: Lens.Lens' DescribeModelPackageGroupResponse (Prelude.Maybe Prelude.Text)
describeModelPackageGroupResponse_modelPackageGroupDescription = Lens.lens (\DescribeModelPackageGroupResponse' {modelPackageGroupDescription} -> modelPackageGroupDescription) (\s@DescribeModelPackageGroupResponse' {} a -> s {modelPackageGroupDescription = a} :: DescribeModelPackageGroupResponse)

-- | The response's http status code.
describeModelPackageGroupResponse_httpStatus :: Lens.Lens' DescribeModelPackageGroupResponse Prelude.Int
describeModelPackageGroupResponse_httpStatus = Lens.lens (\DescribeModelPackageGroupResponse' {httpStatus} -> httpStatus) (\s@DescribeModelPackageGroupResponse' {} a -> s {httpStatus = a} :: DescribeModelPackageGroupResponse)

-- | The name of the model group.
describeModelPackageGroupResponse_modelPackageGroupName :: Lens.Lens' DescribeModelPackageGroupResponse Prelude.Text
describeModelPackageGroupResponse_modelPackageGroupName = Lens.lens (\DescribeModelPackageGroupResponse' {modelPackageGroupName} -> modelPackageGroupName) (\s@DescribeModelPackageGroupResponse' {} a -> s {modelPackageGroupName = a} :: DescribeModelPackageGroupResponse)

-- | The Amazon Resource Name (ARN) of the model group.
describeModelPackageGroupResponse_modelPackageGroupArn :: Lens.Lens' DescribeModelPackageGroupResponse Prelude.Text
describeModelPackageGroupResponse_modelPackageGroupArn = Lens.lens (\DescribeModelPackageGroupResponse' {modelPackageGroupArn} -> modelPackageGroupArn) (\s@DescribeModelPackageGroupResponse' {} a -> s {modelPackageGroupArn = a} :: DescribeModelPackageGroupResponse)

-- | The time that the model group was created.
describeModelPackageGroupResponse_creationTime :: Lens.Lens' DescribeModelPackageGroupResponse Prelude.UTCTime
describeModelPackageGroupResponse_creationTime = Lens.lens (\DescribeModelPackageGroupResponse' {creationTime} -> creationTime) (\s@DescribeModelPackageGroupResponse' {} a -> s {creationTime = a} :: DescribeModelPackageGroupResponse) Prelude.. Data._Time

-- | Undocumented member.
describeModelPackageGroupResponse_createdBy :: Lens.Lens' DescribeModelPackageGroupResponse UserContext
describeModelPackageGroupResponse_createdBy = Lens.lens (\DescribeModelPackageGroupResponse' {createdBy} -> createdBy) (\s@DescribeModelPackageGroupResponse' {} a -> s {createdBy = a} :: DescribeModelPackageGroupResponse)

-- | The status of the model group.
describeModelPackageGroupResponse_modelPackageGroupStatus :: Lens.Lens' DescribeModelPackageGroupResponse ModelPackageGroupStatus
describeModelPackageGroupResponse_modelPackageGroupStatus = Lens.lens (\DescribeModelPackageGroupResponse' {modelPackageGroupStatus} -> modelPackageGroupStatus) (\s@DescribeModelPackageGroupResponse' {} a -> s {modelPackageGroupStatus = a} :: DescribeModelPackageGroupResponse)

instance
  Prelude.NFData
    DescribeModelPackageGroupResponse
  where
  rnf DescribeModelPackageGroupResponse' {..} =
    Prelude.rnf modelPackageGroupDescription
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf modelPackageGroupName
      `Prelude.seq` Prelude.rnf modelPackageGroupArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf modelPackageGroupStatus
