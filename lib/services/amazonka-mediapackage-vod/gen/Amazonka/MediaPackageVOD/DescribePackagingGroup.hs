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
-- Module      : Amazonka.MediaPackageVOD.DescribePackagingGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of a MediaPackage VOD PackagingGroup resource.
module Amazonka.MediaPackageVOD.DescribePackagingGroup
  ( -- * Creating a Request
    DescribePackagingGroup (..),
    newDescribePackagingGroup,

    -- * Request Lenses
    describePackagingGroup_id,

    -- * Destructuring the Response
    DescribePackagingGroupResponse (..),
    newDescribePackagingGroupResponse,

    -- * Response Lenses
    describePackagingGroupResponse_approximateAssetCount,
    describePackagingGroupResponse_arn,
    describePackagingGroupResponse_authorization,
    describePackagingGroupResponse_domainName,
    describePackagingGroupResponse_egressAccessLogs,
    describePackagingGroupResponse_id,
    describePackagingGroupResponse_tags,
    describePackagingGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageVOD.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribePackagingGroup' smart constructor.
data DescribePackagingGroup = DescribePackagingGroup'
  { -- | The ID of a MediaPackage VOD PackagingGroup resource.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePackagingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'describePackagingGroup_id' - The ID of a MediaPackage VOD PackagingGroup resource.
newDescribePackagingGroup ::
  -- | 'id'
  Prelude.Text ->
  DescribePackagingGroup
newDescribePackagingGroup pId_ =
  DescribePackagingGroup' {id = pId_}

-- | The ID of a MediaPackage VOD PackagingGroup resource.
describePackagingGroup_id :: Lens.Lens' DescribePackagingGroup Prelude.Text
describePackagingGroup_id = Lens.lens (\DescribePackagingGroup' {id} -> id) (\s@DescribePackagingGroup' {} a -> s {id = a} :: DescribePackagingGroup)

instance Core.AWSRequest DescribePackagingGroup where
  type
    AWSResponse DescribePackagingGroup =
      DescribePackagingGroupResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePackagingGroupResponse'
            Prelude.<$> (x Data..?> "approximateAssetCount")
            Prelude.<*> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "authorization")
            Prelude.<*> (x Data..?> "domainName")
            Prelude.<*> (x Data..?> "egressAccessLogs")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePackagingGroup where
  hashWithSalt _salt DescribePackagingGroup' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DescribePackagingGroup where
  rnf DescribePackagingGroup' {..} = Prelude.rnf id

instance Data.ToHeaders DescribePackagingGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribePackagingGroup where
  toPath DescribePackagingGroup' {..} =
    Prelude.mconcat
      ["/packaging_groups/", Data.toBS id]

instance Data.ToQuery DescribePackagingGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePackagingGroupResponse' smart constructor.
data DescribePackagingGroupResponse = DescribePackagingGroupResponse'
  { -- | The approximate asset count of the PackagingGroup.
    approximateAssetCount :: Prelude.Maybe Prelude.Int,
    -- | The ARN of the PackagingGroup.
    arn :: Prelude.Maybe Prelude.Text,
    authorization :: Prelude.Maybe Authorization,
    -- | The fully qualified domain name for Assets in the PackagingGroup.
    domainName :: Prelude.Maybe Prelude.Text,
    egressAccessLogs :: Prelude.Maybe EgressAccessLogs,
    -- | The ID of the PackagingGroup.
    id :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePackagingGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'approximateAssetCount', 'describePackagingGroupResponse_approximateAssetCount' - The approximate asset count of the PackagingGroup.
--
-- 'arn', 'describePackagingGroupResponse_arn' - The ARN of the PackagingGroup.
--
-- 'authorization', 'describePackagingGroupResponse_authorization' - Undocumented member.
--
-- 'domainName', 'describePackagingGroupResponse_domainName' - The fully qualified domain name for Assets in the PackagingGroup.
--
-- 'egressAccessLogs', 'describePackagingGroupResponse_egressAccessLogs' - Undocumented member.
--
-- 'id', 'describePackagingGroupResponse_id' - The ID of the PackagingGroup.
--
-- 'tags', 'describePackagingGroupResponse_tags' - Undocumented member.
--
-- 'httpStatus', 'describePackagingGroupResponse_httpStatus' - The response's http status code.
newDescribePackagingGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePackagingGroupResponse
newDescribePackagingGroupResponse pHttpStatus_ =
  DescribePackagingGroupResponse'
    { approximateAssetCount =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      authorization = Prelude.Nothing,
      domainName = Prelude.Nothing,
      egressAccessLogs = Prelude.Nothing,
      id = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The approximate asset count of the PackagingGroup.
describePackagingGroupResponse_approximateAssetCount :: Lens.Lens' DescribePackagingGroupResponse (Prelude.Maybe Prelude.Int)
describePackagingGroupResponse_approximateAssetCount = Lens.lens (\DescribePackagingGroupResponse' {approximateAssetCount} -> approximateAssetCount) (\s@DescribePackagingGroupResponse' {} a -> s {approximateAssetCount = a} :: DescribePackagingGroupResponse)

-- | The ARN of the PackagingGroup.
describePackagingGroupResponse_arn :: Lens.Lens' DescribePackagingGroupResponse (Prelude.Maybe Prelude.Text)
describePackagingGroupResponse_arn = Lens.lens (\DescribePackagingGroupResponse' {arn} -> arn) (\s@DescribePackagingGroupResponse' {} a -> s {arn = a} :: DescribePackagingGroupResponse)

-- | Undocumented member.
describePackagingGroupResponse_authorization :: Lens.Lens' DescribePackagingGroupResponse (Prelude.Maybe Authorization)
describePackagingGroupResponse_authorization = Lens.lens (\DescribePackagingGroupResponse' {authorization} -> authorization) (\s@DescribePackagingGroupResponse' {} a -> s {authorization = a} :: DescribePackagingGroupResponse)

-- | The fully qualified domain name for Assets in the PackagingGroup.
describePackagingGroupResponse_domainName :: Lens.Lens' DescribePackagingGroupResponse (Prelude.Maybe Prelude.Text)
describePackagingGroupResponse_domainName = Lens.lens (\DescribePackagingGroupResponse' {domainName} -> domainName) (\s@DescribePackagingGroupResponse' {} a -> s {domainName = a} :: DescribePackagingGroupResponse)

-- | Undocumented member.
describePackagingGroupResponse_egressAccessLogs :: Lens.Lens' DescribePackagingGroupResponse (Prelude.Maybe EgressAccessLogs)
describePackagingGroupResponse_egressAccessLogs = Lens.lens (\DescribePackagingGroupResponse' {egressAccessLogs} -> egressAccessLogs) (\s@DescribePackagingGroupResponse' {} a -> s {egressAccessLogs = a} :: DescribePackagingGroupResponse)

-- | The ID of the PackagingGroup.
describePackagingGroupResponse_id :: Lens.Lens' DescribePackagingGroupResponse (Prelude.Maybe Prelude.Text)
describePackagingGroupResponse_id = Lens.lens (\DescribePackagingGroupResponse' {id} -> id) (\s@DescribePackagingGroupResponse' {} a -> s {id = a} :: DescribePackagingGroupResponse)

-- | Undocumented member.
describePackagingGroupResponse_tags :: Lens.Lens' DescribePackagingGroupResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describePackagingGroupResponse_tags = Lens.lens (\DescribePackagingGroupResponse' {tags} -> tags) (\s@DescribePackagingGroupResponse' {} a -> s {tags = a} :: DescribePackagingGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describePackagingGroupResponse_httpStatus :: Lens.Lens' DescribePackagingGroupResponse Prelude.Int
describePackagingGroupResponse_httpStatus = Lens.lens (\DescribePackagingGroupResponse' {httpStatus} -> httpStatus) (\s@DescribePackagingGroupResponse' {} a -> s {httpStatus = a} :: DescribePackagingGroupResponse)

instance
  Prelude.NFData
    DescribePackagingGroupResponse
  where
  rnf DescribePackagingGroupResponse' {..} =
    Prelude.rnf approximateAssetCount
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf authorization
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf egressAccessLogs
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
