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
-- Module      : Amazonka.Inspector.Types.RulesPackage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector.Types.RulesPackage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an Amazon Inspector rules package. This data
-- type is used as the response element in the DescribeRulesPackages
-- action.
--
-- /See:/ 'newRulesPackage' smart constructor.
data RulesPackage = RulesPackage'
  { -- | The description of the rules package.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the rules package.
    arn :: Prelude.Text,
    -- | The name of the rules package.
    name :: Prelude.Text,
    -- | The version ID of the rules package.
    version :: Prelude.Text,
    -- | The provider of the rules package.
    provider :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RulesPackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'rulesPackage_description' - The description of the rules package.
--
-- 'arn', 'rulesPackage_arn' - The ARN of the rules package.
--
-- 'name', 'rulesPackage_name' - The name of the rules package.
--
-- 'version', 'rulesPackage_version' - The version ID of the rules package.
--
-- 'provider', 'rulesPackage_provider' - The provider of the rules package.
newRulesPackage ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'version'
  Prelude.Text ->
  -- | 'provider'
  Prelude.Text ->
  RulesPackage
newRulesPackage pArn_ pName_ pVersion_ pProvider_ =
  RulesPackage'
    { description = Prelude.Nothing,
      arn = pArn_,
      name = pName_,
      version = pVersion_,
      provider = pProvider_
    }

-- | The description of the rules package.
rulesPackage_description :: Lens.Lens' RulesPackage (Prelude.Maybe Prelude.Text)
rulesPackage_description = Lens.lens (\RulesPackage' {description} -> description) (\s@RulesPackage' {} a -> s {description = a} :: RulesPackage)

-- | The ARN of the rules package.
rulesPackage_arn :: Lens.Lens' RulesPackage Prelude.Text
rulesPackage_arn = Lens.lens (\RulesPackage' {arn} -> arn) (\s@RulesPackage' {} a -> s {arn = a} :: RulesPackage)

-- | The name of the rules package.
rulesPackage_name :: Lens.Lens' RulesPackage Prelude.Text
rulesPackage_name = Lens.lens (\RulesPackage' {name} -> name) (\s@RulesPackage' {} a -> s {name = a} :: RulesPackage)

-- | The version ID of the rules package.
rulesPackage_version :: Lens.Lens' RulesPackage Prelude.Text
rulesPackage_version = Lens.lens (\RulesPackage' {version} -> version) (\s@RulesPackage' {} a -> s {version = a} :: RulesPackage)

-- | The provider of the rules package.
rulesPackage_provider :: Lens.Lens' RulesPackage Prelude.Text
rulesPackage_provider = Lens.lens (\RulesPackage' {provider} -> provider) (\s@RulesPackage' {} a -> s {provider = a} :: RulesPackage)

instance Data.FromJSON RulesPackage where
  parseJSON =
    Data.withObject
      "RulesPackage"
      ( \x ->
          RulesPackage'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "version")
            Prelude.<*> (x Data..: "provider")
      )

instance Prelude.Hashable RulesPackage where
  hashWithSalt _salt RulesPackage' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` provider

instance Prelude.NFData RulesPackage where
  rnf RulesPackage' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf provider
