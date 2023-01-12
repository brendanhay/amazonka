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
-- Module      : Amazonka.SageMaker.DescribeLineageGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of properties for the requested lineage group. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/xaccount-lineage-tracking.html Cross-Account Lineage Tracking>
-- in the /Amazon SageMaker Developer Guide/.
module Amazonka.SageMaker.DescribeLineageGroup
  ( -- * Creating a Request
    DescribeLineageGroup (..),
    newDescribeLineageGroup,

    -- * Request Lenses
    describeLineageGroup_lineageGroupName,

    -- * Destructuring the Response
    DescribeLineageGroupResponse (..),
    newDescribeLineageGroupResponse,

    -- * Response Lenses
    describeLineageGroupResponse_createdBy,
    describeLineageGroupResponse_creationTime,
    describeLineageGroupResponse_description,
    describeLineageGroupResponse_displayName,
    describeLineageGroupResponse_lastModifiedBy,
    describeLineageGroupResponse_lastModifiedTime,
    describeLineageGroupResponse_lineageGroupArn,
    describeLineageGroupResponse_lineageGroupName,
    describeLineageGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeLineageGroup' smart constructor.
data DescribeLineageGroup = DescribeLineageGroup'
  { -- | The name of the lineage group.
    lineageGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLineageGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lineageGroupName', 'describeLineageGroup_lineageGroupName' - The name of the lineage group.
newDescribeLineageGroup ::
  -- | 'lineageGroupName'
  Prelude.Text ->
  DescribeLineageGroup
newDescribeLineageGroup pLineageGroupName_ =
  DescribeLineageGroup'
    { lineageGroupName =
        pLineageGroupName_
    }

-- | The name of the lineage group.
describeLineageGroup_lineageGroupName :: Lens.Lens' DescribeLineageGroup Prelude.Text
describeLineageGroup_lineageGroupName = Lens.lens (\DescribeLineageGroup' {lineageGroupName} -> lineageGroupName) (\s@DescribeLineageGroup' {} a -> s {lineageGroupName = a} :: DescribeLineageGroup)

instance Core.AWSRequest DescribeLineageGroup where
  type
    AWSResponse DescribeLineageGroup =
      DescribeLineageGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLineageGroupResponse'
            Prelude.<$> (x Data..?> "CreatedBy")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "DisplayName")
            Prelude.<*> (x Data..?> "LastModifiedBy")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "LineageGroupArn")
            Prelude.<*> (x Data..?> "LineageGroupName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeLineageGroup where
  hashWithSalt _salt DescribeLineageGroup' {..} =
    _salt `Prelude.hashWithSalt` lineageGroupName

instance Prelude.NFData DescribeLineageGroup where
  rnf DescribeLineageGroup' {..} =
    Prelude.rnf lineageGroupName

instance Data.ToHeaders DescribeLineageGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DescribeLineageGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeLineageGroup where
  toJSON DescribeLineageGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("LineageGroupName" Data..= lineageGroupName)
          ]
      )

instance Data.ToPath DescribeLineageGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeLineageGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeLineageGroupResponse' smart constructor.
data DescribeLineageGroupResponse = DescribeLineageGroupResponse'
  { createdBy :: Prelude.Maybe UserContext,
    -- | The creation time of lineage group.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the lineage group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The display name of the lineage group.
    displayName :: Prelude.Maybe Prelude.Text,
    lastModifiedBy :: Prelude.Maybe UserContext,
    -- | The last modified time of the lineage group.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the lineage group.
    lineageGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the lineage group.
    lineageGroupName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLineageGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdBy', 'describeLineageGroupResponse_createdBy' - Undocumented member.
--
-- 'creationTime', 'describeLineageGroupResponse_creationTime' - The creation time of lineage group.
--
-- 'description', 'describeLineageGroupResponse_description' - The description of the lineage group.
--
-- 'displayName', 'describeLineageGroupResponse_displayName' - The display name of the lineage group.
--
-- 'lastModifiedBy', 'describeLineageGroupResponse_lastModifiedBy' - Undocumented member.
--
-- 'lastModifiedTime', 'describeLineageGroupResponse_lastModifiedTime' - The last modified time of the lineage group.
--
-- 'lineageGroupArn', 'describeLineageGroupResponse_lineageGroupArn' - The Amazon Resource Name (ARN) of the lineage group.
--
-- 'lineageGroupName', 'describeLineageGroupResponse_lineageGroupName' - The name of the lineage group.
--
-- 'httpStatus', 'describeLineageGroupResponse_httpStatus' - The response's http status code.
newDescribeLineageGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLineageGroupResponse
newDescribeLineageGroupResponse pHttpStatus_ =
  DescribeLineageGroupResponse'
    { createdBy =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      description = Prelude.Nothing,
      displayName = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      lineageGroupArn = Prelude.Nothing,
      lineageGroupName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeLineageGroupResponse_createdBy :: Lens.Lens' DescribeLineageGroupResponse (Prelude.Maybe UserContext)
describeLineageGroupResponse_createdBy = Lens.lens (\DescribeLineageGroupResponse' {createdBy} -> createdBy) (\s@DescribeLineageGroupResponse' {} a -> s {createdBy = a} :: DescribeLineageGroupResponse)

-- | The creation time of lineage group.
describeLineageGroupResponse_creationTime :: Lens.Lens' DescribeLineageGroupResponse (Prelude.Maybe Prelude.UTCTime)
describeLineageGroupResponse_creationTime = Lens.lens (\DescribeLineageGroupResponse' {creationTime} -> creationTime) (\s@DescribeLineageGroupResponse' {} a -> s {creationTime = a} :: DescribeLineageGroupResponse) Prelude.. Lens.mapping Data._Time

-- | The description of the lineage group.
describeLineageGroupResponse_description :: Lens.Lens' DescribeLineageGroupResponse (Prelude.Maybe Prelude.Text)
describeLineageGroupResponse_description = Lens.lens (\DescribeLineageGroupResponse' {description} -> description) (\s@DescribeLineageGroupResponse' {} a -> s {description = a} :: DescribeLineageGroupResponse)

-- | The display name of the lineage group.
describeLineageGroupResponse_displayName :: Lens.Lens' DescribeLineageGroupResponse (Prelude.Maybe Prelude.Text)
describeLineageGroupResponse_displayName = Lens.lens (\DescribeLineageGroupResponse' {displayName} -> displayName) (\s@DescribeLineageGroupResponse' {} a -> s {displayName = a} :: DescribeLineageGroupResponse)

-- | Undocumented member.
describeLineageGroupResponse_lastModifiedBy :: Lens.Lens' DescribeLineageGroupResponse (Prelude.Maybe UserContext)
describeLineageGroupResponse_lastModifiedBy = Lens.lens (\DescribeLineageGroupResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribeLineageGroupResponse' {} a -> s {lastModifiedBy = a} :: DescribeLineageGroupResponse)

-- | The last modified time of the lineage group.
describeLineageGroupResponse_lastModifiedTime :: Lens.Lens' DescribeLineageGroupResponse (Prelude.Maybe Prelude.UTCTime)
describeLineageGroupResponse_lastModifiedTime = Lens.lens (\DescribeLineageGroupResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeLineageGroupResponse' {} a -> s {lastModifiedTime = a} :: DescribeLineageGroupResponse) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the lineage group.
describeLineageGroupResponse_lineageGroupArn :: Lens.Lens' DescribeLineageGroupResponse (Prelude.Maybe Prelude.Text)
describeLineageGroupResponse_lineageGroupArn = Lens.lens (\DescribeLineageGroupResponse' {lineageGroupArn} -> lineageGroupArn) (\s@DescribeLineageGroupResponse' {} a -> s {lineageGroupArn = a} :: DescribeLineageGroupResponse)

-- | The name of the lineage group.
describeLineageGroupResponse_lineageGroupName :: Lens.Lens' DescribeLineageGroupResponse (Prelude.Maybe Prelude.Text)
describeLineageGroupResponse_lineageGroupName = Lens.lens (\DescribeLineageGroupResponse' {lineageGroupName} -> lineageGroupName) (\s@DescribeLineageGroupResponse' {} a -> s {lineageGroupName = a} :: DescribeLineageGroupResponse)

-- | The response's http status code.
describeLineageGroupResponse_httpStatus :: Lens.Lens' DescribeLineageGroupResponse Prelude.Int
describeLineageGroupResponse_httpStatus = Lens.lens (\DescribeLineageGroupResponse' {httpStatus} -> httpStatus) (\s@DescribeLineageGroupResponse' {} a -> s {httpStatus = a} :: DescribeLineageGroupResponse)

instance Prelude.NFData DescribeLineageGroupResponse where
  rnf DescribeLineageGroupResponse' {..} =
    Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf lineageGroupArn
      `Prelude.seq` Prelude.rnf lineageGroupName
      `Prelude.seq` Prelude.rnf httpStatus
