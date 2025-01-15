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
-- Module      : Amazonka.IoT.DescribeThingGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe a thing group.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DescribeThingGroup>
-- action.
module Amazonka.IoT.DescribeThingGroup
  ( -- * Creating a Request
    DescribeThingGroup (..),
    newDescribeThingGroup,

    -- * Request Lenses
    describeThingGroup_thingGroupName,

    -- * Destructuring the Response
    DescribeThingGroupResponse (..),
    newDescribeThingGroupResponse,

    -- * Response Lenses
    describeThingGroupResponse_indexName,
    describeThingGroupResponse_queryString,
    describeThingGroupResponse_queryVersion,
    describeThingGroupResponse_status,
    describeThingGroupResponse_thingGroupArn,
    describeThingGroupResponse_thingGroupId,
    describeThingGroupResponse_thingGroupMetadata,
    describeThingGroupResponse_thingGroupName,
    describeThingGroupResponse_thingGroupProperties,
    describeThingGroupResponse_version,
    describeThingGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeThingGroupResponse'
            Prelude.<$> (x Data..?> "indexName")
            Prelude.<*> (x Data..?> "queryString")
            Prelude.<*> (x Data..?> "queryVersion")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "thingGroupArn")
            Prelude.<*> (x Data..?> "thingGroupId")
            Prelude.<*> (x Data..?> "thingGroupMetadata")
            Prelude.<*> (x Data..?> "thingGroupName")
            Prelude.<*> (x Data..?> "thingGroupProperties")
            Prelude.<*> (x Data..?> "version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeThingGroup where
  hashWithSalt _salt DescribeThingGroup' {..} =
    _salt `Prelude.hashWithSalt` thingGroupName

instance Prelude.NFData DescribeThingGroup where
  rnf DescribeThingGroup' {..} =
    Prelude.rnf thingGroupName

instance Data.ToHeaders DescribeThingGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeThingGroup where
  toPath DescribeThingGroup' {..} =
    Prelude.mconcat
      ["/thing-groups/", Data.toBS thingGroupName]

instance Data.ToQuery DescribeThingGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeThingGroupResponse' smart constructor.
data DescribeThingGroupResponse = DescribeThingGroupResponse'
  { -- | The dynamic thing group index name.
    indexName :: Prelude.Maybe Prelude.Text,
    -- | The dynamic thing group search query string.
    queryString :: Prelude.Maybe Prelude.Text,
    -- | The dynamic thing group query version.
    queryVersion :: Prelude.Maybe Prelude.Text,
    -- | The dynamic thing group status.
    status :: Prelude.Maybe DynamicGroupStatus,
    -- | The thing group ARN.
    thingGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The thing group ID.
    thingGroupId :: Prelude.Maybe Prelude.Text,
    -- | Thing group metadata.
    thingGroupMetadata :: Prelude.Maybe ThingGroupMetadata,
    -- | The name of the thing group.
    thingGroupName :: Prelude.Maybe Prelude.Text,
    -- | The thing group properties.
    thingGroupProperties :: Prelude.Maybe ThingGroupProperties,
    -- | The version of the thing group.
    version :: Prelude.Maybe Prelude.Integer,
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
-- 'indexName', 'describeThingGroupResponse_indexName' - The dynamic thing group index name.
--
-- 'queryString', 'describeThingGroupResponse_queryString' - The dynamic thing group search query string.
--
-- 'queryVersion', 'describeThingGroupResponse_queryVersion' - The dynamic thing group query version.
--
-- 'status', 'describeThingGroupResponse_status' - The dynamic thing group status.
--
-- 'thingGroupArn', 'describeThingGroupResponse_thingGroupArn' - The thing group ARN.
--
-- 'thingGroupId', 'describeThingGroupResponse_thingGroupId' - The thing group ID.
--
-- 'thingGroupMetadata', 'describeThingGroupResponse_thingGroupMetadata' - Thing group metadata.
--
-- 'thingGroupName', 'describeThingGroupResponse_thingGroupName' - The name of the thing group.
--
-- 'thingGroupProperties', 'describeThingGroupResponse_thingGroupProperties' - The thing group properties.
--
-- 'version', 'describeThingGroupResponse_version' - The version of the thing group.
--
-- 'httpStatus', 'describeThingGroupResponse_httpStatus' - The response's http status code.
newDescribeThingGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeThingGroupResponse
newDescribeThingGroupResponse pHttpStatus_ =
  DescribeThingGroupResponse'
    { indexName =
        Prelude.Nothing,
      queryString = Prelude.Nothing,
      queryVersion = Prelude.Nothing,
      status = Prelude.Nothing,
      thingGroupArn = Prelude.Nothing,
      thingGroupId = Prelude.Nothing,
      thingGroupMetadata = Prelude.Nothing,
      thingGroupName = Prelude.Nothing,
      thingGroupProperties = Prelude.Nothing,
      version = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The dynamic thing group index name.
describeThingGroupResponse_indexName :: Lens.Lens' DescribeThingGroupResponse (Prelude.Maybe Prelude.Text)
describeThingGroupResponse_indexName = Lens.lens (\DescribeThingGroupResponse' {indexName} -> indexName) (\s@DescribeThingGroupResponse' {} a -> s {indexName = a} :: DescribeThingGroupResponse)

-- | The dynamic thing group search query string.
describeThingGroupResponse_queryString :: Lens.Lens' DescribeThingGroupResponse (Prelude.Maybe Prelude.Text)
describeThingGroupResponse_queryString = Lens.lens (\DescribeThingGroupResponse' {queryString} -> queryString) (\s@DescribeThingGroupResponse' {} a -> s {queryString = a} :: DescribeThingGroupResponse)

-- | The dynamic thing group query version.
describeThingGroupResponse_queryVersion :: Lens.Lens' DescribeThingGroupResponse (Prelude.Maybe Prelude.Text)
describeThingGroupResponse_queryVersion = Lens.lens (\DescribeThingGroupResponse' {queryVersion} -> queryVersion) (\s@DescribeThingGroupResponse' {} a -> s {queryVersion = a} :: DescribeThingGroupResponse)

-- | The dynamic thing group status.
describeThingGroupResponse_status :: Lens.Lens' DescribeThingGroupResponse (Prelude.Maybe DynamicGroupStatus)
describeThingGroupResponse_status = Lens.lens (\DescribeThingGroupResponse' {status} -> status) (\s@DescribeThingGroupResponse' {} a -> s {status = a} :: DescribeThingGroupResponse)

-- | The thing group ARN.
describeThingGroupResponse_thingGroupArn :: Lens.Lens' DescribeThingGroupResponse (Prelude.Maybe Prelude.Text)
describeThingGroupResponse_thingGroupArn = Lens.lens (\DescribeThingGroupResponse' {thingGroupArn} -> thingGroupArn) (\s@DescribeThingGroupResponse' {} a -> s {thingGroupArn = a} :: DescribeThingGroupResponse)

-- | The thing group ID.
describeThingGroupResponse_thingGroupId :: Lens.Lens' DescribeThingGroupResponse (Prelude.Maybe Prelude.Text)
describeThingGroupResponse_thingGroupId = Lens.lens (\DescribeThingGroupResponse' {thingGroupId} -> thingGroupId) (\s@DescribeThingGroupResponse' {} a -> s {thingGroupId = a} :: DescribeThingGroupResponse)

-- | Thing group metadata.
describeThingGroupResponse_thingGroupMetadata :: Lens.Lens' DescribeThingGroupResponse (Prelude.Maybe ThingGroupMetadata)
describeThingGroupResponse_thingGroupMetadata = Lens.lens (\DescribeThingGroupResponse' {thingGroupMetadata} -> thingGroupMetadata) (\s@DescribeThingGroupResponse' {} a -> s {thingGroupMetadata = a} :: DescribeThingGroupResponse)

-- | The name of the thing group.
describeThingGroupResponse_thingGroupName :: Lens.Lens' DescribeThingGroupResponse (Prelude.Maybe Prelude.Text)
describeThingGroupResponse_thingGroupName = Lens.lens (\DescribeThingGroupResponse' {thingGroupName} -> thingGroupName) (\s@DescribeThingGroupResponse' {} a -> s {thingGroupName = a} :: DescribeThingGroupResponse)

-- | The thing group properties.
describeThingGroupResponse_thingGroupProperties :: Lens.Lens' DescribeThingGroupResponse (Prelude.Maybe ThingGroupProperties)
describeThingGroupResponse_thingGroupProperties = Lens.lens (\DescribeThingGroupResponse' {thingGroupProperties} -> thingGroupProperties) (\s@DescribeThingGroupResponse' {} a -> s {thingGroupProperties = a} :: DescribeThingGroupResponse)

-- | The version of the thing group.
describeThingGroupResponse_version :: Lens.Lens' DescribeThingGroupResponse (Prelude.Maybe Prelude.Integer)
describeThingGroupResponse_version = Lens.lens (\DescribeThingGroupResponse' {version} -> version) (\s@DescribeThingGroupResponse' {} a -> s {version = a} :: DescribeThingGroupResponse)

-- | The response's http status code.
describeThingGroupResponse_httpStatus :: Lens.Lens' DescribeThingGroupResponse Prelude.Int
describeThingGroupResponse_httpStatus = Lens.lens (\DescribeThingGroupResponse' {httpStatus} -> httpStatus) (\s@DescribeThingGroupResponse' {} a -> s {httpStatus = a} :: DescribeThingGroupResponse)

instance Prelude.NFData DescribeThingGroupResponse where
  rnf DescribeThingGroupResponse' {..} =
    Prelude.rnf indexName `Prelude.seq`
      Prelude.rnf queryString `Prelude.seq`
        Prelude.rnf queryVersion `Prelude.seq`
          Prelude.rnf status `Prelude.seq`
            Prelude.rnf thingGroupArn `Prelude.seq`
              Prelude.rnf thingGroupId `Prelude.seq`
                Prelude.rnf thingGroupMetadata `Prelude.seq`
                  Prelude.rnf thingGroupName `Prelude.seq`
                    Prelude.rnf thingGroupProperties `Prelude.seq`
                      Prelude.rnf version `Prelude.seq`
                        Prelude.rnf httpStatus
