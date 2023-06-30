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
-- Module      : Amazonka.ElastiCache.CreateCacheSecurityGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new cache security group. Use a cache security group to
-- control access to one or more clusters.
--
-- Cache security groups are only used when you are creating a cluster
-- outside of an Amazon Virtual Private Cloud (Amazon VPC). If you are
-- creating a cluster inside of a VPC, use a cache subnet group instead.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_CreateCacheSubnetGroup.html CreateCacheSubnetGroup>.
module Amazonka.ElastiCache.CreateCacheSecurityGroup
  ( -- * Creating a Request
    CreateCacheSecurityGroup (..),
    newCreateCacheSecurityGroup,

    -- * Request Lenses
    createCacheSecurityGroup_tags,
    createCacheSecurityGroup_cacheSecurityGroupName,
    createCacheSecurityGroup_description,

    -- * Destructuring the Response
    CreateCacheSecurityGroupResponse (..),
    newCreateCacheSecurityGroupResponse,

    -- * Response Lenses
    createCacheSecurityGroupResponse_cacheSecurityGroup,
    createCacheSecurityGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @CreateCacheSecurityGroup@ operation.
--
-- /See:/ 'newCreateCacheSecurityGroup' smart constructor.
data CreateCacheSecurityGroup = CreateCacheSecurityGroup'
  { -- | A list of tags to be added to this resource. A tag is a key-value pair.
    -- A tag key must be accompanied by a tag value, although null is accepted.
    tags :: Prelude.Maybe [Tag],
    -- | A name for the cache security group. This value is stored as a lowercase
    -- string.
    --
    -- Constraints: Must contain no more than 255 alphanumeric characters.
    -- Cannot be the word \"Default\".
    --
    -- Example: @mysecuritygroup@
    cacheSecurityGroupName :: Prelude.Text,
    -- | A description for the cache security group.
    description :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCacheSecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createCacheSecurityGroup_tags' - A list of tags to be added to this resource. A tag is a key-value pair.
-- A tag key must be accompanied by a tag value, although null is accepted.
--
-- 'cacheSecurityGroupName', 'createCacheSecurityGroup_cacheSecurityGroupName' - A name for the cache security group. This value is stored as a lowercase
-- string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters.
-- Cannot be the word \"Default\".
--
-- Example: @mysecuritygroup@
--
-- 'description', 'createCacheSecurityGroup_description' - A description for the cache security group.
newCreateCacheSecurityGroup ::
  -- | 'cacheSecurityGroupName'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  CreateCacheSecurityGroup
newCreateCacheSecurityGroup
  pCacheSecurityGroupName_
  pDescription_ =
    CreateCacheSecurityGroup'
      { tags = Prelude.Nothing,
        cacheSecurityGroupName = pCacheSecurityGroupName_,
        description = pDescription_
      }

-- | A list of tags to be added to this resource. A tag is a key-value pair.
-- A tag key must be accompanied by a tag value, although null is accepted.
createCacheSecurityGroup_tags :: Lens.Lens' CreateCacheSecurityGroup (Prelude.Maybe [Tag])
createCacheSecurityGroup_tags = Lens.lens (\CreateCacheSecurityGroup' {tags} -> tags) (\s@CreateCacheSecurityGroup' {} a -> s {tags = a} :: CreateCacheSecurityGroup) Prelude.. Lens.mapping Lens.coerced

-- | A name for the cache security group. This value is stored as a lowercase
-- string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters.
-- Cannot be the word \"Default\".
--
-- Example: @mysecuritygroup@
createCacheSecurityGroup_cacheSecurityGroupName :: Lens.Lens' CreateCacheSecurityGroup Prelude.Text
createCacheSecurityGroup_cacheSecurityGroupName = Lens.lens (\CreateCacheSecurityGroup' {cacheSecurityGroupName} -> cacheSecurityGroupName) (\s@CreateCacheSecurityGroup' {} a -> s {cacheSecurityGroupName = a} :: CreateCacheSecurityGroup)

-- | A description for the cache security group.
createCacheSecurityGroup_description :: Lens.Lens' CreateCacheSecurityGroup Prelude.Text
createCacheSecurityGroup_description = Lens.lens (\CreateCacheSecurityGroup' {description} -> description) (\s@CreateCacheSecurityGroup' {} a -> s {description = a} :: CreateCacheSecurityGroup)

instance Core.AWSRequest CreateCacheSecurityGroup where
  type
    AWSResponse CreateCacheSecurityGroup =
      CreateCacheSecurityGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateCacheSecurityGroupResult"
      ( \s h x ->
          CreateCacheSecurityGroupResponse'
            Prelude.<$> (x Data..@? "CacheSecurityGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCacheSecurityGroup where
  hashWithSalt _salt CreateCacheSecurityGroup' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` cacheSecurityGroupName
      `Prelude.hashWithSalt` description

instance Prelude.NFData CreateCacheSecurityGroup where
  rnf CreateCacheSecurityGroup' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf cacheSecurityGroupName
      `Prelude.seq` Prelude.rnf description

instance Data.ToHeaders CreateCacheSecurityGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateCacheSecurityGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateCacheSecurityGroup where
  toQuery CreateCacheSecurityGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateCacheSecurityGroup" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-02-02" :: Prelude.ByteString),
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "Tag" Prelude.<$> tags),
        "CacheSecurityGroupName"
          Data.=: cacheSecurityGroupName,
        "Description" Data.=: description
      ]

-- | /See:/ 'newCreateCacheSecurityGroupResponse' smart constructor.
data CreateCacheSecurityGroupResponse = CreateCacheSecurityGroupResponse'
  { cacheSecurityGroup :: Prelude.Maybe CacheSecurityGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCacheSecurityGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheSecurityGroup', 'createCacheSecurityGroupResponse_cacheSecurityGroup' - Undocumented member.
--
-- 'httpStatus', 'createCacheSecurityGroupResponse_httpStatus' - The response's http status code.
newCreateCacheSecurityGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCacheSecurityGroupResponse
newCreateCacheSecurityGroupResponse pHttpStatus_ =
  CreateCacheSecurityGroupResponse'
    { cacheSecurityGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createCacheSecurityGroupResponse_cacheSecurityGroup :: Lens.Lens' CreateCacheSecurityGroupResponse (Prelude.Maybe CacheSecurityGroup)
createCacheSecurityGroupResponse_cacheSecurityGroup = Lens.lens (\CreateCacheSecurityGroupResponse' {cacheSecurityGroup} -> cacheSecurityGroup) (\s@CreateCacheSecurityGroupResponse' {} a -> s {cacheSecurityGroup = a} :: CreateCacheSecurityGroupResponse)

-- | The response's http status code.
createCacheSecurityGroupResponse_httpStatus :: Lens.Lens' CreateCacheSecurityGroupResponse Prelude.Int
createCacheSecurityGroupResponse_httpStatus = Lens.lens (\CreateCacheSecurityGroupResponse' {httpStatus} -> httpStatus) (\s@CreateCacheSecurityGroupResponse' {} a -> s {httpStatus = a} :: CreateCacheSecurityGroupResponse)

instance
  Prelude.NFData
    CreateCacheSecurityGroupResponse
  where
  rnf CreateCacheSecurityGroupResponse' {..} =
    Prelude.rnf cacheSecurityGroup
      `Prelude.seq` Prelude.rnf httpStatus
