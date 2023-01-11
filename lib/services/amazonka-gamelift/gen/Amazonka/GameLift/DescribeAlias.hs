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
-- Module      : Amazonka.GameLift.DescribeAlias
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- __Related actions__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
module Amazonka.GameLift.DescribeAlias
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAlias' smart constructor.
data DescribeAlias = DescribeAlias'
  { -- | The unique identifier for the fleet alias that you want to retrieve. You
    -- can use either the alias ID or ARN value.
    aliasId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeAlias
newDescribeAlias pAliasId_ =
  DescribeAlias' {aliasId = pAliasId_}

-- | The unique identifier for the fleet alias that you want to retrieve. You
-- can use either the alias ID or ARN value.
describeAlias_aliasId :: Lens.Lens' DescribeAlias Prelude.Text
describeAlias_aliasId = Lens.lens (\DescribeAlias' {aliasId} -> aliasId) (\s@DescribeAlias' {} a -> s {aliasId = a} :: DescribeAlias)

instance Core.AWSRequest DescribeAlias where
  type
    AWSResponse DescribeAlias =
      DescribeAliasResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAliasResponse'
            Prelude.<$> (x Data..?> "Alias")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAlias where
  hashWithSalt _salt DescribeAlias' {..} =
    _salt `Prelude.hashWithSalt` aliasId

instance Prelude.NFData DescribeAlias where
  rnf DescribeAlias' {..} = Prelude.rnf aliasId

instance Data.ToHeaders DescribeAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("GameLift.DescribeAlias" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeAlias where
  toJSON DescribeAlias' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("AliasId" Data..= aliasId)]
      )

instance Data.ToPath DescribeAlias where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAliasResponse' smart constructor.
data DescribeAliasResponse = DescribeAliasResponse'
  { -- | The requested alias resource.
    alias :: Prelude.Maybe Alias,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeAliasResponse
newDescribeAliasResponse pHttpStatus_ =
  DescribeAliasResponse'
    { alias = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The requested alias resource.
describeAliasResponse_alias :: Lens.Lens' DescribeAliasResponse (Prelude.Maybe Alias)
describeAliasResponse_alias = Lens.lens (\DescribeAliasResponse' {alias} -> alias) (\s@DescribeAliasResponse' {} a -> s {alias = a} :: DescribeAliasResponse)

-- | The response's http status code.
describeAliasResponse_httpStatus :: Lens.Lens' DescribeAliasResponse Prelude.Int
describeAliasResponse_httpStatus = Lens.lens (\DescribeAliasResponse' {httpStatus} -> httpStatus) (\s@DescribeAliasResponse' {} a -> s {httpStatus = a} :: DescribeAliasResponse)

instance Prelude.NFData DescribeAliasResponse where
  rnf DescribeAliasResponse' {..} =
    Prelude.rnf alias
      `Prelude.seq` Prelude.rnf httpStatus
