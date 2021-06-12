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
-- Module      : Network.AWS.GameLift.DescribeAlias
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves properties for an alias. This operation returns all alias
-- metadata and settings. To get an alias\'s target fleet ID only, use
-- @ResolveAlias@.
--
-- To get alias properties, specify the alias ID. If successful, the
-- requested alias record is returned.
--
-- -   CreateAlias
--
-- -   ListAliases
--
-- -   DescribeAlias
--
-- -   UpdateAlias
--
-- -   DeleteAlias
--
-- -   ResolveAlias
module Network.AWS.GameLift.DescribeAlias
  ( -- * Creating a Request
    DescribeAlias (..),
    newDescribeAlias,

    -- * Request Lenses
    describeAlias_aliasId,

    -- * Destructuring the Response
    DescribeAliasResponse (..),
    newDescribeAliasResponse,

    -- * Response Lenses
    describeAliasResponse_alias,
    describeAliasResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDescribeAlias' smart constructor.
data DescribeAlias = DescribeAlias'
  { -- | The unique identifier for the fleet alias that you want to retrieve. You
    -- can use either the alias ID or ARN value.
    aliasId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aliasId', 'describeAlias_aliasId' - The unique identifier for the fleet alias that you want to retrieve. You
-- can use either the alias ID or ARN value.
newDescribeAlias ::
  -- | 'aliasId'
  Core.Text ->
  DescribeAlias
newDescribeAlias pAliasId_ =
  DescribeAlias' {aliasId = pAliasId_}

-- | The unique identifier for the fleet alias that you want to retrieve. You
-- can use either the alias ID or ARN value.
describeAlias_aliasId :: Lens.Lens' DescribeAlias Core.Text
describeAlias_aliasId = Lens.lens (\DescribeAlias' {aliasId} -> aliasId) (\s@DescribeAlias' {} a -> s {aliasId = a} :: DescribeAlias)

instance Core.AWSRequest DescribeAlias where
  type
    AWSResponse DescribeAlias =
      DescribeAliasResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAliasResponse'
            Core.<$> (x Core..?> "Alias")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeAlias

instance Core.NFData DescribeAlias

instance Core.ToHeaders DescribeAlias where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.DescribeAlias" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeAlias where
  toJSON DescribeAlias' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("AliasId" Core..= aliasId)]
      )

instance Core.ToPath DescribeAlias where
  toPath = Core.const "/"

instance Core.ToQuery DescribeAlias where
  toQuery = Core.const Core.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newDescribeAliasResponse' smart constructor.
data DescribeAliasResponse = DescribeAliasResponse'
  { -- | The requested alias resource.
    alias :: Core.Maybe Alias,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alias', 'describeAliasResponse_alias' - The requested alias resource.
--
-- 'httpStatus', 'describeAliasResponse_httpStatus' - The response's http status code.
newDescribeAliasResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeAliasResponse
newDescribeAliasResponse pHttpStatus_ =
  DescribeAliasResponse'
    { alias = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The requested alias resource.
describeAliasResponse_alias :: Lens.Lens' DescribeAliasResponse (Core.Maybe Alias)
describeAliasResponse_alias = Lens.lens (\DescribeAliasResponse' {alias} -> alias) (\s@DescribeAliasResponse' {} a -> s {alias = a} :: DescribeAliasResponse)

-- | The response's http status code.
describeAliasResponse_httpStatus :: Lens.Lens' DescribeAliasResponse Core.Int
describeAliasResponse_httpStatus = Lens.lens (\DescribeAliasResponse' {httpStatus} -> httpStatus) (\s@DescribeAliasResponse' {} a -> s {httpStatus = a} :: DescribeAliasResponse)

instance Core.NFData DescribeAliasResponse
