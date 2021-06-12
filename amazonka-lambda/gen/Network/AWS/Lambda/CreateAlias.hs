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
-- Module      : Network.AWS.Lambda.CreateAlias
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an
-- <https://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html alias>
-- for a Lambda function version. Use aliases to provide clients with a
-- function identifier that you can update to invoke a different version.
--
-- You can also map an alias to split invocation requests between two
-- versions. Use the @RoutingConfig@ parameter to specify a second version
-- and the percentage of invocation requests that it receives.
module Network.AWS.Lambda.CreateAlias
  ( -- * Creating a Request
    CreateAlias (..),
    newCreateAlias,

    -- * Request Lenses
    createAlias_routingConfig,
    createAlias_description,
    createAlias_functionName,
    createAlias_name,
    createAlias_functionVersion,

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

-- | /See:/ 'newCreateAlias' smart constructor.
data CreateAlias = CreateAlias'
  { -- | The
    -- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-aliases.html#configuring-alias-routing routing configuration>
    -- of the alias.
    routingConfig :: Core.Maybe AliasRoutingConfiguration,
    -- | A description of the alias.
    description :: Core.Maybe Core.Text,
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
    functionName :: Core.Text,
    -- | The name of the alias.
    name :: Core.Text,
    -- | The function version that the alias invokes.
    functionVersion :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'routingConfig', 'createAlias_routingConfig' - The
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-aliases.html#configuring-alias-routing routing configuration>
-- of the alias.
--
-- 'description', 'createAlias_description' - A description of the alias.
--
-- 'functionName', 'createAlias_functionName' - The name of the Lambda function.
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
-- 'name', 'createAlias_name' - The name of the alias.
--
-- 'functionVersion', 'createAlias_functionVersion' - The function version that the alias invokes.
newCreateAlias ::
  -- | 'functionName'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  -- | 'functionVersion'
  Core.Text ->
  CreateAlias
newCreateAlias
  pFunctionName_
  pName_
  pFunctionVersion_ =
    CreateAlias'
      { routingConfig = Core.Nothing,
        description = Core.Nothing,
        functionName = pFunctionName_,
        name = pName_,
        functionVersion = pFunctionVersion_
      }

-- | The
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-aliases.html#configuring-alias-routing routing configuration>
-- of the alias.
createAlias_routingConfig :: Lens.Lens' CreateAlias (Core.Maybe AliasRoutingConfiguration)
createAlias_routingConfig = Lens.lens (\CreateAlias' {routingConfig} -> routingConfig) (\s@CreateAlias' {} a -> s {routingConfig = a} :: CreateAlias)

-- | A description of the alias.
createAlias_description :: Lens.Lens' CreateAlias (Core.Maybe Core.Text)
createAlias_description = Lens.lens (\CreateAlias' {description} -> description) (\s@CreateAlias' {} a -> s {description = a} :: CreateAlias)

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
createAlias_functionName :: Lens.Lens' CreateAlias Core.Text
createAlias_functionName = Lens.lens (\CreateAlias' {functionName} -> functionName) (\s@CreateAlias' {} a -> s {functionName = a} :: CreateAlias)

-- | The name of the alias.
createAlias_name :: Lens.Lens' CreateAlias Core.Text
createAlias_name = Lens.lens (\CreateAlias' {name} -> name) (\s@CreateAlias' {} a -> s {name = a} :: CreateAlias)

-- | The function version that the alias invokes.
createAlias_functionVersion :: Lens.Lens' CreateAlias Core.Text
createAlias_functionVersion = Lens.lens (\CreateAlias' {functionVersion} -> functionVersion) (\s@CreateAlias' {} a -> s {functionVersion = a} :: CreateAlias)

instance Core.AWSRequest CreateAlias where
  type AWSResponse CreateAlias = AliasConfiguration
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable CreateAlias

instance Core.NFData CreateAlias

instance Core.ToHeaders CreateAlias where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON CreateAlias where
  toJSON CreateAlias' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RoutingConfig" Core..=) Core.<$> routingConfig,
            ("Description" Core..=) Core.<$> description,
            Core.Just ("Name" Core..= name),
            Core.Just
              ("FunctionVersion" Core..= functionVersion)
          ]
      )

instance Core.ToPath CreateAlias where
  toPath CreateAlias' {..} =
    Core.mconcat
      [ "/2015-03-31/functions/",
        Core.toBS functionName,
        "/aliases"
      ]

instance Core.ToQuery CreateAlias where
  toQuery = Core.const Core.mempty
