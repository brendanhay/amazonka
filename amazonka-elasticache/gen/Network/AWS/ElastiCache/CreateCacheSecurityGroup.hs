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
-- Module      : Network.AWS.ElastiCache.CreateCacheSecurityGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.ElastiCache.CreateCacheSecurityGroup
  ( -- * Creating a Request
    CreateCacheSecurityGroup (..),
    newCreateCacheSecurityGroup,

    -- * Request Lenses
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

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @CreateCacheSecurityGroup@ operation.
--
-- /See:/ 'newCreateCacheSecurityGroup' smart constructor.
data CreateCacheSecurityGroup = CreateCacheSecurityGroup'
  { -- | A name for the cache security group. This value is stored as a lowercase
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
      { cacheSecurityGroupName =
          pCacheSecurityGroupName_,
        description = pDescription_
      }

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
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateCacheSecurityGroupResult"
      ( \s h x ->
          CreateCacheSecurityGroupResponse'
            Prelude.<$> (x Core..@? "CacheSecurityGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCacheSecurityGroup

instance Prelude.NFData CreateCacheSecurityGroup

instance Core.ToHeaders CreateCacheSecurityGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateCacheSecurityGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateCacheSecurityGroup where
  toQuery CreateCacheSecurityGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateCacheSecurityGroup" :: Prelude.ByteString),
        "Version"
          Core.=: ("2015-02-02" :: Prelude.ByteString),
        "CacheSecurityGroupName"
          Core.=: cacheSecurityGroupName,
        "Description" Core.=: description
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
