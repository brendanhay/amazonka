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
-- Module      : Amazonka.Lambda.CreateAlias
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.Lambda.CreateAlias
  ( -- * Creating a Request
    CreateAlias (..),
    newCreateAlias,

    -- * Request Lenses
    createAlias_description,
    createAlias_routingConfig,
    createAlias_functionName,
    createAlias_name,
    createAlias_functionVersion,

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

-- | /See:/ 'newCreateAlias' smart constructor.
data CreateAlias = CreateAlias'
  { -- | A description of the alias.
    description :: Prelude.Maybe Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-aliases.html#configuring-alias-routing routing configuration>
    -- of the alias.
    routingConfig :: Prelude.Maybe AliasRoutingConfiguration,
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
    functionName :: Prelude.Text,
    -- | The name of the alias.
    name :: Prelude.Text,
    -- | The function version that the alias invokes.
    functionVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createAlias_description' - A description of the alias.
--
-- 'routingConfig', 'createAlias_routingConfig' - The
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-aliases.html#configuring-alias-routing routing configuration>
-- of the alias.
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
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'functionVersion'
  Prelude.Text ->
  CreateAlias
newCreateAlias
  pFunctionName_
  pName_
  pFunctionVersion_ =
    CreateAlias'
      { description = Prelude.Nothing,
        routingConfig = Prelude.Nothing,
        functionName = pFunctionName_,
        name = pName_,
        functionVersion = pFunctionVersion_
      }

-- | A description of the alias.
createAlias_description :: Lens.Lens' CreateAlias (Prelude.Maybe Prelude.Text)
createAlias_description = Lens.lens (\CreateAlias' {description} -> description) (\s@CreateAlias' {} a -> s {description = a} :: CreateAlias)

-- | The
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-aliases.html#configuring-alias-routing routing configuration>
-- of the alias.
createAlias_routingConfig :: Lens.Lens' CreateAlias (Prelude.Maybe AliasRoutingConfiguration)
createAlias_routingConfig = Lens.lens (\CreateAlias' {routingConfig} -> routingConfig) (\s@CreateAlias' {} a -> s {routingConfig = a} :: CreateAlias)

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
createAlias_functionName :: Lens.Lens' CreateAlias Prelude.Text
createAlias_functionName = Lens.lens (\CreateAlias' {functionName} -> functionName) (\s@CreateAlias' {} a -> s {functionName = a} :: CreateAlias)

-- | The name of the alias.
createAlias_name :: Lens.Lens' CreateAlias Prelude.Text
createAlias_name = Lens.lens (\CreateAlias' {name} -> name) (\s@CreateAlias' {} a -> s {name = a} :: CreateAlias)

-- | The function version that the alias invokes.
createAlias_functionVersion :: Lens.Lens' CreateAlias Prelude.Text
createAlias_functionVersion = Lens.lens (\CreateAlias' {functionVersion} -> functionVersion) (\s@CreateAlias' {} a -> s {functionVersion = a} :: CreateAlias)

instance Core.AWSRequest CreateAlias where
  type AWSResponse CreateAlias = AliasConfiguration
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable CreateAlias where
  hashWithSalt _salt CreateAlias' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` routingConfig
      `Prelude.hashWithSalt` functionName
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` functionVersion

instance Prelude.NFData CreateAlias where
  rnf CreateAlias' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf routingConfig
      `Prelude.seq` Prelude.rnf functionName
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf functionVersion

instance Data.ToHeaders CreateAlias where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateAlias where
  toJSON CreateAlias' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("RoutingConfig" Data..=) Prelude.<$> routingConfig,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("FunctionVersion" Data..= functionVersion)
          ]
      )

instance Data.ToPath CreateAlias where
  toPath CreateAlias' {..} =
    Prelude.mconcat
      [ "/2015-03-31/functions/",
        Data.toBS functionName,
        "/aliases"
      ]

instance Data.ToQuery CreateAlias where
  toQuery = Prelude.const Prelude.mempty
