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
-- Module      : Network.AWS.IoT.DescribeThingGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe a thing group.
module Network.AWS.IoT.DescribeThingGroup
  ( -- * Creating a Request
    DescribeThingGroup (..),
    newDescribeThingGroup,

    -- * Request Lenses
    describeThingGroup_thingGroupName,

    -- * Destructuring the Response
    DescribeThingGroupResponse (..),
    newDescribeThingGroupResponse,

    -- * Response Lenses
    describeThingGroupResponse_queryString,
    describeThingGroupResponse_status,
    describeThingGroupResponse_indexName,
    describeThingGroupResponse_thingGroupArn,
    describeThingGroupResponse_queryVersion,
    describeThingGroupResponse_version,
    describeThingGroupResponse_thingGroupName,
    describeThingGroupResponse_thingGroupId,
    describeThingGroupResponse_thingGroupMetadata,
    describeThingGroupResponse_thingGroupProperties,
    describeThingGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeThingGroup' smart constructor.
data DescribeThingGroup = DescribeThingGroup'
  { -- | The name of the thing group.
    thingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeThingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thingGroupName', 'describeThingGroup_thingGroupName' - The name of the thing group.
newDescribeThingGroup ::
  -- | 'thingGroupName'
  Prelude.Text ->
  DescribeThingGroup
newDescribeThingGroup pThingGroupName_ =
  DescribeThingGroup'
    { thingGroupName =
        pThingGroupName_
    }

-- | The name of the thing group.
describeThingGroup_thingGroupName :: Lens.Lens' DescribeThingGroup Prelude.Text
describeThingGroup_thingGroupName = Lens.lens (\DescribeThingGroup' {thingGroupName} -> thingGroupName) (\s@DescribeThingGroup' {} a -> s {thingGroupName = a} :: DescribeThingGroup)

instance Core.AWSRequest DescribeThingGroup where
  type
    AWSResponse DescribeThingGroup =
      DescribeThingGroupResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeThingGroupResponse'
            Prelude.<$> (x Core..?> "queryString")
            Prelude.<*> (x Core..?> "status")
            Prelude.<*> (x Core..?> "indexName")
            Prelude.<*> (x Core..?> "thingGroupArn")
            Prelude.<*> (x Core..?> "queryVersion")
            Prelude.<*> (x Core..?> "version")
            Prelude.<*> (x Core..?> "thingGroupName")
            Prelude.<*> (x Core..?> "thingGroupId")
            Prelude.<*> (x Core..?> "thingGroupMetadata")
            Prelude.<*> (x Core..?> "thingGroupProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeThingGroup

instance Prelude.NFData DescribeThingGroup

instance Core.ToHeaders DescribeThingGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeThingGroup where
  toPath DescribeThingGroup' {..} =
    Prelude.mconcat
      ["/thing-groups/", Core.toBS thingGroupName]

instance Core.ToQuery DescribeThingGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeThingGroupResponse' smart constructor.
data DescribeThingGroupResponse = DescribeThingGroupResponse'
  { -- | The dynamic thing group search query string.
    queryString :: Prelude.Maybe Prelude.Text,
    -- | The dynamic thing group status.
    status :: Prelude.Maybe DynamicGroupStatus,
    -- | The dynamic thing group index name.
    indexName :: Prelude.Maybe Prelude.Text,
    -- | The thing group ARN.
    thingGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The dynamic thing group query version.
    queryVersion :: Prelude.Maybe Prelude.Text,
    -- | The version of the thing group.
    version :: Prelude.Maybe Prelude.Integer,
    -- | The name of the thing group.
    thingGroupName :: Prelude.Maybe Prelude.Text,
    -- | The thing group ID.
    thingGroupId :: Prelude.Maybe Prelude.Text,
    -- | Thing group metadata.
    thingGroupMetadata :: Prelude.Maybe ThingGroupMetadata,
    -- | The thing group properties.
    thingGroupProperties :: Prelude.Maybe ThingGroupProperties,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeThingGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryString', 'describeThingGroupResponse_queryString' - The dynamic thing group search query string.
--
-- 'status', 'describeThingGroupResponse_status' - The dynamic thing group status.
--
-- 'indexName', 'describeThingGroupResponse_indexName' - The dynamic thing group index name.
--
-- 'thingGroupArn', 'describeThingGroupResponse_thingGroupArn' - The thing group ARN.
--
-- 'queryVersion', 'describeThingGroupResponse_queryVersion' - The dynamic thing group query version.
--
-- 'version', 'describeThingGroupResponse_version' - The version of the thing group.
--
-- 'thingGroupName', 'describeThingGroupResponse_thingGroupName' - The name of the thing group.
--
-- 'thingGroupId', 'describeThingGroupResponse_thingGroupId' - The thing group ID.
--
-- 'thingGroupMetadata', 'describeThingGroupResponse_thingGroupMetadata' - Thing group metadata.
--
-- 'thingGroupProperties', 'describeThingGroupResponse_thingGroupProperties' - The thing group properties.
--
-- 'httpStatus', 'describeThingGroupResponse_httpStatus' - The response's http status code.
newDescribeThingGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeThingGroupResponse
newDescribeThingGroupResponse pHttpStatus_ =
  DescribeThingGroupResponse'
    { queryString =
        Prelude.Nothing,
      status = Prelude.Nothing,
      indexName = Prelude.Nothing,
      thingGroupArn = Prelude.Nothing,
      queryVersion = Prelude.Nothing,
      version = Prelude.Nothing,
      thingGroupName = Prelude.Nothing,
      thingGroupId = Prelude.Nothing,
      thingGroupMetadata = Prelude.Nothing,
      thingGroupProperties = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The dynamic thing group search query string.
describeThingGroupResponse_queryString :: Lens.Lens' DescribeThingGroupResponse (Prelude.Maybe Prelude.Text)
describeThingGroupResponse_queryString = Lens.lens (\DescribeThingGroupResponse' {queryString} -> queryString) (\s@DescribeThingGroupResponse' {} a -> s {queryString = a} :: DescribeThingGroupResponse)

-- | The dynamic thing group status.
describeThingGroupResponse_status :: Lens.Lens' DescribeThingGroupResponse (Prelude.Maybe DynamicGroupStatus)
describeThingGroupResponse_status = Lens.lens (\DescribeThingGroupResponse' {status} -> status) (\s@DescribeThingGroupResponse' {} a -> s {status = a} :: DescribeThingGroupResponse)

-- | The dynamic thing group index name.
describeThingGroupResponse_indexName :: Lens.Lens' DescribeThingGroupResponse (Prelude.Maybe Prelude.Text)
describeThingGroupResponse_indexName = Lens.lens (\DescribeThingGroupResponse' {indexName} -> indexName) (\s@DescribeThingGroupResponse' {} a -> s {indexName = a} :: DescribeThingGroupResponse)

-- | The thing group ARN.
describeThingGroupResponse_thingGroupArn :: Lens.Lens' DescribeThingGroupResponse (Prelude.Maybe Prelude.Text)
describeThingGroupResponse_thingGroupArn = Lens.lens (\DescribeThingGroupResponse' {thingGroupArn} -> thingGroupArn) (\s@DescribeThingGroupResponse' {} a -> s {thingGroupArn = a} :: DescribeThingGroupResponse)

-- | The dynamic thing group query version.
describeThingGroupResponse_queryVersion :: Lens.Lens' DescribeThingGroupResponse (Prelude.Maybe Prelude.Text)
describeThingGroupResponse_queryVersion = Lens.lens (\DescribeThingGroupResponse' {queryVersion} -> queryVersion) (\s@DescribeThingGroupResponse' {} a -> s {queryVersion = a} :: DescribeThingGroupResponse)

-- | The version of the thing group.
describeThingGroupResponse_version :: Lens.Lens' DescribeThingGroupResponse (Prelude.Maybe Prelude.Integer)
describeThingGroupResponse_version = Lens.lens (\DescribeThingGroupResponse' {version} -> version) (\s@DescribeThingGroupResponse' {} a -> s {version = a} :: DescribeThingGroupResponse)

-- | The name of the thing group.
describeThingGroupResponse_thingGroupName :: Lens.Lens' DescribeThingGroupResponse (Prelude.Maybe Prelude.Text)
describeThingGroupResponse_thingGroupName = Lens.lens (\DescribeThingGroupResponse' {thingGroupName} -> thingGroupName) (\s@DescribeThingGroupResponse' {} a -> s {thingGroupName = a} :: DescribeThingGroupResponse)

-- | The thing group ID.
describeThingGroupResponse_thingGroupId :: Lens.Lens' DescribeThingGroupResponse (Prelude.Maybe Prelude.Text)
describeThingGroupResponse_thingGroupId = Lens.lens (\DescribeThingGroupResponse' {thingGroupId} -> thingGroupId) (\s@DescribeThingGroupResponse' {} a -> s {thingGroupId = a} :: DescribeThingGroupResponse)

-- | Thing group metadata.
describeThingGroupResponse_thingGroupMetadata :: Lens.Lens' DescribeThingGroupResponse (Prelude.Maybe ThingGroupMetadata)
describeThingGroupResponse_thingGroupMetadata = Lens.lens (\DescribeThingGroupResponse' {thingGroupMetadata} -> thingGroupMetadata) (\s@DescribeThingGroupResponse' {} a -> s {thingGroupMetadata = a} :: DescribeThingGroupResponse)

-- | The thing group properties.
describeThingGroupResponse_thingGroupProperties :: Lens.Lens' DescribeThingGroupResponse (Prelude.Maybe ThingGroupProperties)
describeThingGroupResponse_thingGroupProperties = Lens.lens (\DescribeThingGroupResponse' {thingGroupProperties} -> thingGroupProperties) (\s@DescribeThingGroupResponse' {} a -> s {thingGroupProperties = a} :: DescribeThingGroupResponse)

-- | The response's http status code.
describeThingGroupResponse_httpStatus :: Lens.Lens' DescribeThingGroupResponse Prelude.Int
describeThingGroupResponse_httpStatus = Lens.lens (\DescribeThingGroupResponse' {httpStatus} -> httpStatus) (\s@DescribeThingGroupResponse' {} a -> s {httpStatus = a} :: DescribeThingGroupResponse)

instance Prelude.NFData DescribeThingGroupResponse
