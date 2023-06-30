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
-- Module      : Amazonka.ElastiCache.CreateCacheParameterGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon ElastiCache cache parameter group. An ElastiCache
-- cache parameter group is a collection of parameters and their values
-- that are applied to all of the nodes in any cluster or replication group
-- using the CacheParameterGroup.
--
-- A newly created CacheParameterGroup is an exact duplicate of the default
-- parameter group for the CacheParameterGroupFamily. To customize the
-- newly created CacheParameterGroup you can change the values of specific
-- parameters. For more information, see:
--
-- -   <https://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_ModifyCacheParameterGroup.html ModifyCacheParameterGroup>
--     in the ElastiCache API Reference.
--
-- -   <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/ParameterGroups.html Parameters and Parameter Groups>
--     in the ElastiCache User Guide.
module Amazonka.ElastiCache.CreateCacheParameterGroup
  ( -- * Creating a Request
    CreateCacheParameterGroup (..),
    newCreateCacheParameterGroup,

    -- * Request Lenses
    createCacheParameterGroup_tags,
    createCacheParameterGroup_cacheParameterGroupName,
    createCacheParameterGroup_cacheParameterGroupFamily,
    createCacheParameterGroup_description,

    -- * Destructuring the Response
    CreateCacheParameterGroupResponse (..),
    newCreateCacheParameterGroupResponse,

    -- * Response Lenses
    createCacheParameterGroupResponse_cacheParameterGroup,
    createCacheParameterGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @CreateCacheParameterGroup@ operation.
--
-- /See:/ 'newCreateCacheParameterGroup' smart constructor.
data CreateCacheParameterGroup = CreateCacheParameterGroup'
  { -- | A list of tags to be added to this resource. A tag is a key-value pair.
    -- A tag key must be accompanied by a tag value, although null is accepted.
    tags :: Prelude.Maybe [Tag],
    -- | A user-specified name for the cache parameter group.
    cacheParameterGroupName :: Prelude.Text,
    -- | The name of the cache parameter group family that the cache parameter
    -- group can be used with.
    --
    -- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ |
    -- @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ |
    -- @redis6.x@
    cacheParameterGroupFamily :: Prelude.Text,
    -- | A user-specified description for the cache parameter group.
    description :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCacheParameterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createCacheParameterGroup_tags' - A list of tags to be added to this resource. A tag is a key-value pair.
-- A tag key must be accompanied by a tag value, although null is accepted.
--
-- 'cacheParameterGroupName', 'createCacheParameterGroup_cacheParameterGroupName' - A user-specified name for the cache parameter group.
--
-- 'cacheParameterGroupFamily', 'createCacheParameterGroup_cacheParameterGroupFamily' - The name of the cache parameter group family that the cache parameter
-- group can be used with.
--
-- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ |
-- @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ |
-- @redis6.x@
--
-- 'description', 'createCacheParameterGroup_description' - A user-specified description for the cache parameter group.
newCreateCacheParameterGroup ::
  -- | 'cacheParameterGroupName'
  Prelude.Text ->
  -- | 'cacheParameterGroupFamily'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  CreateCacheParameterGroup
newCreateCacheParameterGroup
  pCacheParameterGroupName_
  pCacheParameterGroupFamily_
  pDescription_ =
    CreateCacheParameterGroup'
      { tags = Prelude.Nothing,
        cacheParameterGroupName =
          pCacheParameterGroupName_,
        cacheParameterGroupFamily =
          pCacheParameterGroupFamily_,
        description = pDescription_
      }

-- | A list of tags to be added to this resource. A tag is a key-value pair.
-- A tag key must be accompanied by a tag value, although null is accepted.
createCacheParameterGroup_tags :: Lens.Lens' CreateCacheParameterGroup (Prelude.Maybe [Tag])
createCacheParameterGroup_tags = Lens.lens (\CreateCacheParameterGroup' {tags} -> tags) (\s@CreateCacheParameterGroup' {} a -> s {tags = a} :: CreateCacheParameterGroup) Prelude.. Lens.mapping Lens.coerced

-- | A user-specified name for the cache parameter group.
createCacheParameterGroup_cacheParameterGroupName :: Lens.Lens' CreateCacheParameterGroup Prelude.Text
createCacheParameterGroup_cacheParameterGroupName = Lens.lens (\CreateCacheParameterGroup' {cacheParameterGroupName} -> cacheParameterGroupName) (\s@CreateCacheParameterGroup' {} a -> s {cacheParameterGroupName = a} :: CreateCacheParameterGroup)

-- | The name of the cache parameter group family that the cache parameter
-- group can be used with.
--
-- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ |
-- @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ |
-- @redis6.x@
createCacheParameterGroup_cacheParameterGroupFamily :: Lens.Lens' CreateCacheParameterGroup Prelude.Text
createCacheParameterGroup_cacheParameterGroupFamily = Lens.lens (\CreateCacheParameterGroup' {cacheParameterGroupFamily} -> cacheParameterGroupFamily) (\s@CreateCacheParameterGroup' {} a -> s {cacheParameterGroupFamily = a} :: CreateCacheParameterGroup)

-- | A user-specified description for the cache parameter group.
createCacheParameterGroup_description :: Lens.Lens' CreateCacheParameterGroup Prelude.Text
createCacheParameterGroup_description = Lens.lens (\CreateCacheParameterGroup' {description} -> description) (\s@CreateCacheParameterGroup' {} a -> s {description = a} :: CreateCacheParameterGroup)

instance Core.AWSRequest CreateCacheParameterGroup where
  type
    AWSResponse CreateCacheParameterGroup =
      CreateCacheParameterGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateCacheParameterGroupResult"
      ( \s h x ->
          CreateCacheParameterGroupResponse'
            Prelude.<$> (x Data..@? "CacheParameterGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCacheParameterGroup where
  hashWithSalt _salt CreateCacheParameterGroup' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` cacheParameterGroupName
      `Prelude.hashWithSalt` cacheParameterGroupFamily
      `Prelude.hashWithSalt` description

instance Prelude.NFData CreateCacheParameterGroup where
  rnf CreateCacheParameterGroup' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf cacheParameterGroupName
      `Prelude.seq` Prelude.rnf cacheParameterGroupFamily
      `Prelude.seq` Prelude.rnf description

instance Data.ToHeaders CreateCacheParameterGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateCacheParameterGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateCacheParameterGroup where
  toQuery CreateCacheParameterGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateCacheParameterGroup" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-02-02" :: Prelude.ByteString),
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "Tag" Prelude.<$> tags),
        "CacheParameterGroupName"
          Data.=: cacheParameterGroupName,
        "CacheParameterGroupFamily"
          Data.=: cacheParameterGroupFamily,
        "Description" Data.=: description
      ]

-- | /See:/ 'newCreateCacheParameterGroupResponse' smart constructor.
data CreateCacheParameterGroupResponse = CreateCacheParameterGroupResponse'
  { cacheParameterGroup :: Prelude.Maybe CacheParameterGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCacheParameterGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheParameterGroup', 'createCacheParameterGroupResponse_cacheParameterGroup' - Undocumented member.
--
-- 'httpStatus', 'createCacheParameterGroupResponse_httpStatus' - The response's http status code.
newCreateCacheParameterGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCacheParameterGroupResponse
newCreateCacheParameterGroupResponse pHttpStatus_ =
  CreateCacheParameterGroupResponse'
    { cacheParameterGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createCacheParameterGroupResponse_cacheParameterGroup :: Lens.Lens' CreateCacheParameterGroupResponse (Prelude.Maybe CacheParameterGroup)
createCacheParameterGroupResponse_cacheParameterGroup = Lens.lens (\CreateCacheParameterGroupResponse' {cacheParameterGroup} -> cacheParameterGroup) (\s@CreateCacheParameterGroupResponse' {} a -> s {cacheParameterGroup = a} :: CreateCacheParameterGroupResponse)

-- | The response's http status code.
createCacheParameterGroupResponse_httpStatus :: Lens.Lens' CreateCacheParameterGroupResponse Prelude.Int
createCacheParameterGroupResponse_httpStatus = Lens.lens (\CreateCacheParameterGroupResponse' {httpStatus} -> httpStatus) (\s@CreateCacheParameterGroupResponse' {} a -> s {httpStatus = a} :: CreateCacheParameterGroupResponse)

instance
  Prelude.NFData
    CreateCacheParameterGroupResponse
  where
  rnf CreateCacheParameterGroupResponse' {..} =
    Prelude.rnf cacheParameterGroup
      `Prelude.seq` Prelude.rnf httpStatus
