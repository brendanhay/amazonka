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
-- Module      : Network.AWS.Lambda.GetAlias
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about a Lambda function
-- <https://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html alias>.
module Network.AWS.Lambda.GetAlias
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
    aliasConfiguration_revisionId,
    aliasConfiguration_routingConfig,
    aliasConfiguration_functionVersion,
    aliasConfiguration_name,
    aliasConfiguration_description,
    aliasConfiguration_aliasArn,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
    functionName :: Core.Text,
    -- | The name of the alias.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'name'
  Core.Text ->
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
getAlias_functionName :: Lens.Lens' GetAlias Core.Text
getAlias_functionName = Lens.lens (\GetAlias' {functionName} -> functionName) (\s@GetAlias' {} a -> s {functionName = a} :: GetAlias)

-- | The name of the alias.
getAlias_name :: Lens.Lens' GetAlias Core.Text
getAlias_name = Lens.lens (\GetAlias' {name} -> name) (\s@GetAlias' {} a -> s {name = a} :: GetAlias)

instance Core.AWSRequest GetAlias where
  type AWSResponse GetAlias = AliasConfiguration
  request = Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable GetAlias

instance Core.NFData GetAlias

instance Core.ToHeaders GetAlias where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetAlias where
  toPath GetAlias' {..} =
    Core.mconcat
      [ "/2015-03-31/functions/",
        Core.toBS functionName,
        "/aliases/",
        Core.toBS name
      ]

instance Core.ToQuery GetAlias where
  toQuery = Core.const Core.mempty
