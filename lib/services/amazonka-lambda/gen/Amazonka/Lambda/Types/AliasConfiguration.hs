{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Lambda.Types.AliasConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.AliasConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types.AliasRoutingConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Provides configuration information about a Lambda function
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-aliases.html alias>.
--
-- /See:/ 'newAliasConfiguration' smart constructor.
data AliasConfiguration = AliasConfiguration'
  { -- | The Amazon Resource Name (ARN) of the alias.
    aliasArn :: Prelude.Maybe Prelude.Text,
    -- | A description of the alias.
    description :: Prelude.Maybe Prelude.Text,
    -- | The function version that the alias invokes.
    functionVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the alias.
    name :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier that changes when you update the alias.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-traffic-shifting-using-aliases.html routing configuration>
    -- of the alias.
    routingConfig :: Prelude.Maybe AliasRoutingConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AliasConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aliasArn', 'aliasConfiguration_aliasArn' - The Amazon Resource Name (ARN) of the alias.
--
-- 'description', 'aliasConfiguration_description' - A description of the alias.
--
-- 'functionVersion', 'aliasConfiguration_functionVersion' - The function version that the alias invokes.
--
-- 'name', 'aliasConfiguration_name' - The name of the alias.
--
-- 'revisionId', 'aliasConfiguration_revisionId' - A unique identifier that changes when you update the alias.
--
-- 'routingConfig', 'aliasConfiguration_routingConfig' - The
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-traffic-shifting-using-aliases.html routing configuration>
-- of the alias.
newAliasConfiguration ::
  AliasConfiguration
newAliasConfiguration =
  AliasConfiguration'
    { aliasArn = Prelude.Nothing,
      description = Prelude.Nothing,
      functionVersion = Prelude.Nothing,
      name = Prelude.Nothing,
      revisionId = Prelude.Nothing,
      routingConfig = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the alias.
aliasConfiguration_aliasArn :: Lens.Lens' AliasConfiguration (Prelude.Maybe Prelude.Text)
aliasConfiguration_aliasArn = Lens.lens (\AliasConfiguration' {aliasArn} -> aliasArn) (\s@AliasConfiguration' {} a -> s {aliasArn = a} :: AliasConfiguration)

-- | A description of the alias.
aliasConfiguration_description :: Lens.Lens' AliasConfiguration (Prelude.Maybe Prelude.Text)
aliasConfiguration_description = Lens.lens (\AliasConfiguration' {description} -> description) (\s@AliasConfiguration' {} a -> s {description = a} :: AliasConfiguration)

-- | The function version that the alias invokes.
aliasConfiguration_functionVersion :: Lens.Lens' AliasConfiguration (Prelude.Maybe Prelude.Text)
aliasConfiguration_functionVersion = Lens.lens (\AliasConfiguration' {functionVersion} -> functionVersion) (\s@AliasConfiguration' {} a -> s {functionVersion = a} :: AliasConfiguration)

-- | The name of the alias.
aliasConfiguration_name :: Lens.Lens' AliasConfiguration (Prelude.Maybe Prelude.Text)
aliasConfiguration_name = Lens.lens (\AliasConfiguration' {name} -> name) (\s@AliasConfiguration' {} a -> s {name = a} :: AliasConfiguration)

-- | A unique identifier that changes when you update the alias.
aliasConfiguration_revisionId :: Lens.Lens' AliasConfiguration (Prelude.Maybe Prelude.Text)
aliasConfiguration_revisionId = Lens.lens (\AliasConfiguration' {revisionId} -> revisionId) (\s@AliasConfiguration' {} a -> s {revisionId = a} :: AliasConfiguration)

-- | The
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-traffic-shifting-using-aliases.html routing configuration>
-- of the alias.
aliasConfiguration_routingConfig :: Lens.Lens' AliasConfiguration (Prelude.Maybe AliasRoutingConfiguration)
aliasConfiguration_routingConfig = Lens.lens (\AliasConfiguration' {routingConfig} -> routingConfig) (\s@AliasConfiguration' {} a -> s {routingConfig = a} :: AliasConfiguration)

instance Data.FromJSON AliasConfiguration where
  parseJSON =
    Data.withObject
      "AliasConfiguration"
      ( \x ->
          AliasConfiguration'
            Prelude.<$> (x Data..:? "AliasArn")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "FunctionVersion")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "RevisionId")
            Prelude.<*> (x Data..:? "RoutingConfig")
      )

instance Prelude.Hashable AliasConfiguration where
  hashWithSalt _salt AliasConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` aliasArn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` functionVersion
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` revisionId
      `Prelude.hashWithSalt` routingConfig

instance Prelude.NFData AliasConfiguration where
  rnf AliasConfiguration' {..} =
    Prelude.rnf aliasArn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf functionVersion
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf routingConfig
