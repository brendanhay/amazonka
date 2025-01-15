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
-- Module      : Amazonka.Lambda.GetAlias
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about a Lambda function
-- <https://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html alias>.
module Amazonka.Lambda.GetAlias
  ( -- * Creating a Request
    GetAlias (..),
    newGetAlias,

    -- * Request Lenses
    getAlias_functionName,
    getAlias_name,

    -- * Destructuring the Response
    AliasConfiguration (..),
    newAliasConfiguration,

    -- * Response Lenses
    aliasConfiguration_aliasArn,
    aliasConfiguration_description,
    aliasConfiguration_functionVersion,
    aliasConfiguration_name,
    aliasConfiguration_revisionId,
    aliasConfiguration_routingConfig,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAlias' smart constructor.
data GetAlias = GetAlias'
  { -- | The name of the Lambda function.
    --
    -- __Name formats__
    --
    -- -   __Function name__ - @MyFunction@.
    --
    -- -   __Function ARN__ -
    --     @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@.
    --
    -- -   __Partial ARN__ - @123456789012:function:MyFunction@.
    --
    -- The length constraint applies only to the full ARN. If you specify only
    -- the function name, it is limited to 64 characters in length.
    functionName :: Prelude.Text,
    -- | The name of the alias.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'functionName', 'getAlias_functionName' - The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ - @MyFunction@.
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@.
--
-- -   __Partial ARN__ - @123456789012:function:MyFunction@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
--
-- 'name', 'getAlias_name' - The name of the alias.
newGetAlias ::
  -- | 'functionName'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  GetAlias
newGetAlias pFunctionName_ pName_ =
  GetAlias'
    { functionName = pFunctionName_,
      name = pName_
    }

-- | The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ - @MyFunction@.
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@.
--
-- -   __Partial ARN__ - @123456789012:function:MyFunction@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
getAlias_functionName :: Lens.Lens' GetAlias Prelude.Text
getAlias_functionName = Lens.lens (\GetAlias' {functionName} -> functionName) (\s@GetAlias' {} a -> s {functionName = a} :: GetAlias)

-- | The name of the alias.
getAlias_name :: Lens.Lens' GetAlias Prelude.Text
getAlias_name = Lens.lens (\GetAlias' {name} -> name) (\s@GetAlias' {} a -> s {name = a} :: GetAlias)

instance Core.AWSRequest GetAlias where
  type AWSResponse GetAlias = AliasConfiguration
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable GetAlias where
  hashWithSalt _salt GetAlias' {..} =
    _salt
      `Prelude.hashWithSalt` functionName
      `Prelude.hashWithSalt` name

instance Prelude.NFData GetAlias where
  rnf GetAlias' {..} =
    Prelude.rnf functionName `Prelude.seq`
      Prelude.rnf name

instance Data.ToHeaders GetAlias where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetAlias where
  toPath GetAlias' {..} =
    Prelude.mconcat
      [ "/2015-03-31/functions/",
        Data.toBS functionName,
        "/aliases/",
        Data.toBS name
      ]

instance Data.ToQuery GetAlias where
  toQuery = Prelude.const Prelude.mempty
