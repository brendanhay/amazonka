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
-- Module      : Network.AWS.MediaPackageVOD.DescribePackagingGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of a MediaPackage VOD PackagingGroup resource.
module Network.AWS.MediaPackageVOD.DescribePackagingGroup
  ( -- * Creating a Request
    DescribePackagingGroup (..),
    newDescribePackagingGroup,

    -- * Request Lenses
    describePackagingGroup_id,

    -- * Destructuring the Response
    DescribePackagingGroupResponse (..),
    newDescribePackagingGroupResponse,

    -- * Response Lenses
    describePackagingGroupResponse_arn,
    describePackagingGroupResponse_authorization,
    describePackagingGroupResponse_domainName,
    describePackagingGroupResponse_id,
    describePackagingGroupResponse_egressAccessLogs,
    describePackagingGroupResponse_tags,
    describePackagingGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackageVOD.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePackagingGroupResponse'
            Prelude.<$> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "authorization")
            Prelude.<*> (x Core..?> "domainName")
            Prelude.<*> (x Core..?> "id")
            Prelude.<*> (x Core..?> "egressAccessLogs")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePackagingGroup

instance Prelude.NFData DescribePackagingGroup

instance Core.ToHeaders DescribePackagingGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribePackagingGroup where
  toPath DescribePackagingGroup' {..} =
    Prelude.mconcat
      ["/packaging_groups/", Core.toBS id]

instance Core.ToQuery DescribePackagingGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePackagingGroupResponse' smart constructor.
data DescribePackagingGroupResponse = DescribePackagingGroupResponse'
  { -- | The ARN of the PackagingGroup.
    arn :: Prelude.Maybe Prelude.Text,
    authorization :: Prelude.Maybe Authorization,
    -- | The fully qualified domain name for Assets in the PackagingGroup.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the PackagingGroup.
    id :: Prelude.Maybe Prelude.Text,
    egressAccessLogs :: Prelude.Maybe EgressAccessLogs,
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
-- 'arn', 'describePackagingGroupResponse_arn' - The ARN of the PackagingGroup.
--
-- 'authorization', 'describePackagingGroupResponse_authorization' - Undocumented member.
--
-- 'domainName', 'describePackagingGroupResponse_domainName' - The fully qualified domain name for Assets in the PackagingGroup.
--
-- 'id', 'describePackagingGroupResponse_id' - The ID of the PackagingGroup.
--
-- 'egressAccessLogs', 'describePackagingGroupResponse_egressAccessLogs' - Undocumented member.
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
    { arn =
        Prelude.Nothing,
      authorization = Prelude.Nothing,
      domainName = Prelude.Nothing,
      id = Prelude.Nothing,
      egressAccessLogs = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the PackagingGroup.
describePackagingGroupResponse_arn :: Lens.Lens' DescribePackagingGroupResponse (Prelude.Maybe Prelude.Text)
describePackagingGroupResponse_arn = Lens.lens (\DescribePackagingGroupResponse' {arn} -> arn) (\s@DescribePackagingGroupResponse' {} a -> s {arn = a} :: DescribePackagingGroupResponse)

-- | Undocumented member.
describePackagingGroupResponse_authorization :: Lens.Lens' DescribePackagingGroupResponse (Prelude.Maybe Authorization)
describePackagingGroupResponse_authorization = Lens.lens (\DescribePackagingGroupResponse' {authorization} -> authorization) (\s@DescribePackagingGroupResponse' {} a -> s {authorization = a} :: DescribePackagingGroupResponse)

-- | The fully qualified domain name for Assets in the PackagingGroup.
describePackagingGroupResponse_domainName :: Lens.Lens' DescribePackagingGroupResponse (Prelude.Maybe Prelude.Text)
describePackagingGroupResponse_domainName = Lens.lens (\DescribePackagingGroupResponse' {domainName} -> domainName) (\s@DescribePackagingGroupResponse' {} a -> s {domainName = a} :: DescribePackagingGroupResponse)

-- | The ID of the PackagingGroup.
describePackagingGroupResponse_id :: Lens.Lens' DescribePackagingGroupResponse (Prelude.Maybe Prelude.Text)
describePackagingGroupResponse_id = Lens.lens (\DescribePackagingGroupResponse' {id} -> id) (\s@DescribePackagingGroupResponse' {} a -> s {id = a} :: DescribePackagingGroupResponse)

-- | Undocumented member.
describePackagingGroupResponse_egressAccessLogs :: Lens.Lens' DescribePackagingGroupResponse (Prelude.Maybe EgressAccessLogs)
describePackagingGroupResponse_egressAccessLogs = Lens.lens (\DescribePackagingGroupResponse' {egressAccessLogs} -> egressAccessLogs) (\s@DescribePackagingGroupResponse' {} a -> s {egressAccessLogs = a} :: DescribePackagingGroupResponse)

-- | Undocumented member.
describePackagingGroupResponse_tags :: Lens.Lens' DescribePackagingGroupResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describePackagingGroupResponse_tags = Lens.lens (\DescribePackagingGroupResponse' {tags} -> tags) (\s@DescribePackagingGroupResponse' {} a -> s {tags = a} :: DescribePackagingGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describePackagingGroupResponse_httpStatus :: Lens.Lens' DescribePackagingGroupResponse Prelude.Int
describePackagingGroupResponse_httpStatus = Lens.lens (\DescribePackagingGroupResponse' {httpStatus} -> httpStatus) (\s@DescribePackagingGroupResponse' {} a -> s {httpStatus = a} :: DescribePackagingGroupResponse)

instance
  Prelude.NFData
    DescribePackagingGroupResponse
