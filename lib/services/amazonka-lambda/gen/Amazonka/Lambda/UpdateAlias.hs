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
-- Module      : Amazonka.Lambda.UpdateAlias
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration of a Lambda function
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-aliases.html alias>.
module Amazonka.Lambda.UpdateAlias
  ( -- * Creating a Request
    UpdateAlias (..),
    newUpdateAlias,

    -- * Request Lenses
    updateAlias_description,
    updateAlias_functionVersion,
    updateAlias_revisionId,
    updateAlias_routingConfig,
    updateAlias_functionName,
    updateAlias_name,

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

-- | /See:/ 'newUpdateAlias' smart constructor.
data UpdateAlias = UpdateAlias'
  { -- | A description of the alias.
    description :: Prelude.Maybe Prelude.Text,
    -- | The function version that the alias invokes.
    functionVersion :: Prelude.Maybe Prelude.Text,
    -- | Only update the alias if the revision ID matches the ID that\'s
    -- specified. Use this option to avoid modifying an alias that has changed
    -- since you last read it.
    revisionId :: Prelude.Maybe Prelude.Text,
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
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateAlias_description' - A description of the alias.
--
-- 'functionVersion', 'updateAlias_functionVersion' - The function version that the alias invokes.
--
-- 'revisionId', 'updateAlias_revisionId' - Only update the alias if the revision ID matches the ID that\'s
-- specified. Use this option to avoid modifying an alias that has changed
-- since you last read it.
--
-- 'routingConfig', 'updateAlias_routingConfig' - The
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-aliases.html#configuring-alias-routing routing configuration>
-- of the alias.
--
-- 'functionName', 'updateAlias_functionName' - The name of the Lambda function.
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
-- 'name', 'updateAlias_name' - The name of the alias.
newUpdateAlias ::
  -- | 'functionName'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  UpdateAlias
newUpdateAlias pFunctionName_ pName_ =
  UpdateAlias'
    { description = Prelude.Nothing,
      functionVersion = Prelude.Nothing,
      revisionId = Prelude.Nothing,
      routingConfig = Prelude.Nothing,
      functionName = pFunctionName_,
      name = pName_
    }

-- | A description of the alias.
updateAlias_description :: Lens.Lens' UpdateAlias (Prelude.Maybe Prelude.Text)
updateAlias_description = Lens.lens (\UpdateAlias' {description} -> description) (\s@UpdateAlias' {} a -> s {description = a} :: UpdateAlias)

-- | The function version that the alias invokes.
updateAlias_functionVersion :: Lens.Lens' UpdateAlias (Prelude.Maybe Prelude.Text)
updateAlias_functionVersion = Lens.lens (\UpdateAlias' {functionVersion} -> functionVersion) (\s@UpdateAlias' {} a -> s {functionVersion = a} :: UpdateAlias)

-- | Only update the alias if the revision ID matches the ID that\'s
-- specified. Use this option to avoid modifying an alias that has changed
-- since you last read it.
updateAlias_revisionId :: Lens.Lens' UpdateAlias (Prelude.Maybe Prelude.Text)
updateAlias_revisionId = Lens.lens (\UpdateAlias' {revisionId} -> revisionId) (\s@UpdateAlias' {} a -> s {revisionId = a} :: UpdateAlias)

-- | The
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-aliases.html#configuring-alias-routing routing configuration>
-- of the alias.
updateAlias_routingConfig :: Lens.Lens' UpdateAlias (Prelude.Maybe AliasRoutingConfiguration)
updateAlias_routingConfig = Lens.lens (\UpdateAlias' {routingConfig} -> routingConfig) (\s@UpdateAlias' {} a -> s {routingConfig = a} :: UpdateAlias)

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
updateAlias_functionName :: Lens.Lens' UpdateAlias Prelude.Text
updateAlias_functionName = Lens.lens (\UpdateAlias' {functionName} -> functionName) (\s@UpdateAlias' {} a -> s {functionName = a} :: UpdateAlias)

-- | The name of the alias.
updateAlias_name :: Lens.Lens' UpdateAlias Prelude.Text
updateAlias_name = Lens.lens (\UpdateAlias' {name} -> name) (\s@UpdateAlias' {} a -> s {name = a} :: UpdateAlias)

instance Core.AWSRequest UpdateAlias where
  type AWSResponse UpdateAlias = AliasConfiguration
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable UpdateAlias where
  hashWithSalt _salt UpdateAlias' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` functionVersion
      `Prelude.hashWithSalt` revisionId
      `Prelude.hashWithSalt` routingConfig
      `Prelude.hashWithSalt` functionName
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateAlias where
  rnf UpdateAlias' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf functionVersion
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf routingConfig
      `Prelude.seq` Prelude.rnf functionName
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders UpdateAlias where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateAlias where
  toJSON UpdateAlias' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("FunctionVersion" Data..=)
              Prelude.<$> functionVersion,
            ("RevisionId" Data..=) Prelude.<$> revisionId,
            ("RoutingConfig" Data..=) Prelude.<$> routingConfig
          ]
      )

instance Data.ToPath UpdateAlias where
  toPath UpdateAlias' {..} =
    Prelude.mconcat
      [ "/2015-03-31/functions/",
        Data.toBS functionName,
        "/aliases/",
        Data.toBS name
      ]

instance Data.ToQuery UpdateAlias where
  toQuery = Prelude.const Prelude.mempty
