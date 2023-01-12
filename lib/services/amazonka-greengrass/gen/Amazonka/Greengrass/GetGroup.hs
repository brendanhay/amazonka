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
-- Module      : Amazonka.Greengrass.GetGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a group.
module Amazonka.Greengrass.GetGroup
  ( -- * Creating a Request
    GetGroup (..),
    newGetGroup,

    -- * Request Lenses
    getGroup_groupId,

    -- * Destructuring the Response
    GetGroupResponse (..),
    newGetGroupResponse,

    -- * Response Lenses
    getGroupResponse_arn,
    getGroupResponse_creationTimestamp,
    getGroupResponse_id,
    getGroupResponse_lastUpdatedTimestamp,
    getGroupResponse_latestVersion,
    getGroupResponse_latestVersionArn,
    getGroupResponse_name,
    getGroupResponse_tags,
    getGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetGroup' smart constructor.
data GetGroup = GetGroup'
  { -- | The ID of the Greengrass group.
    groupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupId', 'getGroup_groupId' - The ID of the Greengrass group.
newGetGroup ::
  -- | 'groupId'
  Prelude.Text ->
  GetGroup
newGetGroup pGroupId_ =
  GetGroup' {groupId = pGroupId_}

-- | The ID of the Greengrass group.
getGroup_groupId :: Lens.Lens' GetGroup Prelude.Text
getGroup_groupId = Lens.lens (\GetGroup' {groupId} -> groupId) (\s@GetGroup' {} a -> s {groupId = a} :: GetGroup)

instance Core.AWSRequest GetGroup where
  type AWSResponse GetGroup = GetGroupResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGroupResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationTimestamp")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "LastUpdatedTimestamp")
            Prelude.<*> (x Data..?> "LatestVersion")
            Prelude.<*> (x Data..?> "LatestVersionArn")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetGroup where
  hashWithSalt _salt GetGroup' {..} =
    _salt `Prelude.hashWithSalt` groupId

instance Prelude.NFData GetGroup where
  rnf GetGroup' {..} = Prelude.rnf groupId

instance Data.ToHeaders GetGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetGroup where
  toPath GetGroup' {..} =
    Prelude.mconcat
      ["/greengrass/groups/", Data.toBS groupId]

instance Data.ToQuery GetGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetGroupResponse' smart constructor.
data GetGroupResponse = GetGroupResponse'
  { -- | The ARN of the definition.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the definition was
    -- created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The ID of the definition.
    id :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the definition was last
    -- updated.
    lastUpdatedTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The ID of the latest version associated with the definition.
    latestVersion :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the latest version associated with the definition.
    latestVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | Tag(s) attached to the resource arn.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getGroupResponse_arn' - The ARN of the definition.
--
-- 'creationTimestamp', 'getGroupResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'id', 'getGroupResponse_id' - The ID of the definition.
--
-- 'lastUpdatedTimestamp', 'getGroupResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'latestVersion', 'getGroupResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'latestVersionArn', 'getGroupResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'name', 'getGroupResponse_name' - The name of the definition.
--
-- 'tags', 'getGroupResponse_tags' - Tag(s) attached to the resource arn.
--
-- 'httpStatus', 'getGroupResponse_httpStatus' - The response's http status code.
newGetGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetGroupResponse
newGetGroupResponse pHttpStatus_ =
  GetGroupResponse'
    { arn = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing,
      latestVersion = Prelude.Nothing,
      latestVersionArn = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the definition.
getGroupResponse_arn :: Lens.Lens' GetGroupResponse (Prelude.Maybe Prelude.Text)
getGroupResponse_arn = Lens.lens (\GetGroupResponse' {arn} -> arn) (\s@GetGroupResponse' {} a -> s {arn = a} :: GetGroupResponse)

-- | The time, in milliseconds since the epoch, when the definition was
-- created.
getGroupResponse_creationTimestamp :: Lens.Lens' GetGroupResponse (Prelude.Maybe Prelude.Text)
getGroupResponse_creationTimestamp = Lens.lens (\GetGroupResponse' {creationTimestamp} -> creationTimestamp) (\s@GetGroupResponse' {} a -> s {creationTimestamp = a} :: GetGroupResponse)

-- | The ID of the definition.
getGroupResponse_id :: Lens.Lens' GetGroupResponse (Prelude.Maybe Prelude.Text)
getGroupResponse_id = Lens.lens (\GetGroupResponse' {id} -> id) (\s@GetGroupResponse' {} a -> s {id = a} :: GetGroupResponse)

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
getGroupResponse_lastUpdatedTimestamp :: Lens.Lens' GetGroupResponse (Prelude.Maybe Prelude.Text)
getGroupResponse_lastUpdatedTimestamp = Lens.lens (\GetGroupResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@GetGroupResponse' {} a -> s {lastUpdatedTimestamp = a} :: GetGroupResponse)

-- | The ID of the latest version associated with the definition.
getGroupResponse_latestVersion :: Lens.Lens' GetGroupResponse (Prelude.Maybe Prelude.Text)
getGroupResponse_latestVersion = Lens.lens (\GetGroupResponse' {latestVersion} -> latestVersion) (\s@GetGroupResponse' {} a -> s {latestVersion = a} :: GetGroupResponse)

-- | The ARN of the latest version associated with the definition.
getGroupResponse_latestVersionArn :: Lens.Lens' GetGroupResponse (Prelude.Maybe Prelude.Text)
getGroupResponse_latestVersionArn = Lens.lens (\GetGroupResponse' {latestVersionArn} -> latestVersionArn) (\s@GetGroupResponse' {} a -> s {latestVersionArn = a} :: GetGroupResponse)

-- | The name of the definition.
getGroupResponse_name :: Lens.Lens' GetGroupResponse (Prelude.Maybe Prelude.Text)
getGroupResponse_name = Lens.lens (\GetGroupResponse' {name} -> name) (\s@GetGroupResponse' {} a -> s {name = a} :: GetGroupResponse)

-- | Tag(s) attached to the resource arn.
getGroupResponse_tags :: Lens.Lens' GetGroupResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getGroupResponse_tags = Lens.lens (\GetGroupResponse' {tags} -> tags) (\s@GetGroupResponse' {} a -> s {tags = a} :: GetGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getGroupResponse_httpStatus :: Lens.Lens' GetGroupResponse Prelude.Int
getGroupResponse_httpStatus = Lens.lens (\GetGroupResponse' {httpStatus} -> httpStatus) (\s@GetGroupResponse' {} a -> s {httpStatus = a} :: GetGroupResponse)

instance Prelude.NFData GetGroupResponse where
  rnf GetGroupResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf latestVersion
      `Prelude.seq` Prelude.rnf latestVersionArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
