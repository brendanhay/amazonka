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
-- Module      : Network.AWS.IoT.DescribeRoleAlias
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a role alias.
module Network.AWS.IoT.DescribeRoleAlias
  ( -- * Creating a Request
    DescribeRoleAlias (..),
    newDescribeRoleAlias,

    -- * Request Lenses
    describeRoleAlias_roleAlias,

    -- * Destructuring the Response
    DescribeRoleAliasResponse (..),
    newDescribeRoleAliasResponse,

    -- * Response Lenses
    describeRoleAliasResponse_roleAliasDescription,
    describeRoleAliasResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeRoleAlias' smart constructor.
data DescribeRoleAlias = DescribeRoleAlias'
  { -- | The role alias to describe.
    roleAlias :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeRoleAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleAlias', 'describeRoleAlias_roleAlias' - The role alias to describe.
newDescribeRoleAlias ::
  -- | 'roleAlias'
  Core.Text ->
  DescribeRoleAlias
newDescribeRoleAlias pRoleAlias_ =
  DescribeRoleAlias' {roleAlias = pRoleAlias_}

-- | The role alias to describe.
describeRoleAlias_roleAlias :: Lens.Lens' DescribeRoleAlias Core.Text
describeRoleAlias_roleAlias = Lens.lens (\DescribeRoleAlias' {roleAlias} -> roleAlias) (\s@DescribeRoleAlias' {} a -> s {roleAlias = a} :: DescribeRoleAlias)

instance Core.AWSRequest DescribeRoleAlias where
  type
    AWSResponse DescribeRoleAlias =
      DescribeRoleAliasResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRoleAliasResponse'
            Core.<$> (x Core..?> "roleAliasDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeRoleAlias

instance Core.NFData DescribeRoleAlias

instance Core.ToHeaders DescribeRoleAlias where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeRoleAlias where
  toPath DescribeRoleAlias' {..} =
    Core.mconcat
      ["/role-aliases/", Core.toBS roleAlias]

instance Core.ToQuery DescribeRoleAlias where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeRoleAliasResponse' smart constructor.
data DescribeRoleAliasResponse = DescribeRoleAliasResponse'
  { -- | The role alias description.
    roleAliasDescription :: Core.Maybe RoleAliasDescription,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeRoleAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleAliasDescription', 'describeRoleAliasResponse_roleAliasDescription' - The role alias description.
--
-- 'httpStatus', 'describeRoleAliasResponse_httpStatus' - The response's http status code.
newDescribeRoleAliasResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeRoleAliasResponse
newDescribeRoleAliasResponse pHttpStatus_ =
  DescribeRoleAliasResponse'
    { roleAliasDescription =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The role alias description.
describeRoleAliasResponse_roleAliasDescription :: Lens.Lens' DescribeRoleAliasResponse (Core.Maybe RoleAliasDescription)
describeRoleAliasResponse_roleAliasDescription = Lens.lens (\DescribeRoleAliasResponse' {roleAliasDescription} -> roleAliasDescription) (\s@DescribeRoleAliasResponse' {} a -> s {roleAliasDescription = a} :: DescribeRoleAliasResponse)

-- | The response's http status code.
describeRoleAliasResponse_httpStatus :: Lens.Lens' DescribeRoleAliasResponse Core.Int
describeRoleAliasResponse_httpStatus = Lens.lens (\DescribeRoleAliasResponse' {httpStatus} -> httpStatus) (\s@DescribeRoleAliasResponse' {} a -> s {httpStatus = a} :: DescribeRoleAliasResponse)

instance Core.NFData DescribeRoleAliasResponse
