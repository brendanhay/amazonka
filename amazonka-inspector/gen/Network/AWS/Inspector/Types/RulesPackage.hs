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
-- Module      : Network.AWS.Inspector.Types.RulesPackage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.RulesPackage where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about an Amazon Inspector rules package. This data
-- type is used as the response element in the DescribeRulesPackages
-- action.
--
-- /See:/ 'newRulesPackage' smart constructor.
data RulesPackage = RulesPackage'
  { -- | The description of the rules package.
    description :: Core.Maybe Core.Text,
    -- | The ARN of the rules package.
    arn :: Core.Text,
    -- | The name of the rules package.
    name :: Core.Text,
    -- | The version ID of the rules package.
    version :: Core.Text,
    -- | The provider of the rules package.
    provider :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'name'
  Core.Text ->
  -- | 'version'
  Core.Text ->
  -- | 'provider'
  Core.Text ->
  RulesPackage
newRulesPackage pArn_ pName_ pVersion_ pProvider_ =
  RulesPackage'
    { description = Core.Nothing,
      arn = pArn_,
      name = pName_,
      version = pVersion_,
      provider = pProvider_
    }

-- | The description of the rules package.
rulesPackage_description :: Lens.Lens' RulesPackage (Core.Maybe Core.Text)
rulesPackage_description = Lens.lens (\RulesPackage' {description} -> description) (\s@RulesPackage' {} a -> s {description = a} :: RulesPackage)

-- | The ARN of the rules package.
rulesPackage_arn :: Lens.Lens' RulesPackage Core.Text
rulesPackage_arn = Lens.lens (\RulesPackage' {arn} -> arn) (\s@RulesPackage' {} a -> s {arn = a} :: RulesPackage)

-- | The name of the rules package.
rulesPackage_name :: Lens.Lens' RulesPackage Core.Text
rulesPackage_name = Lens.lens (\RulesPackage' {name} -> name) (\s@RulesPackage' {} a -> s {name = a} :: RulesPackage)

-- | The version ID of the rules package.
rulesPackage_version :: Lens.Lens' RulesPackage Core.Text
rulesPackage_version = Lens.lens (\RulesPackage' {version} -> version) (\s@RulesPackage' {} a -> s {version = a} :: RulesPackage)

-- | The provider of the rules package.
rulesPackage_provider :: Lens.Lens' RulesPackage Core.Text
rulesPackage_provider = Lens.lens (\RulesPackage' {provider} -> provider) (\s@RulesPackage' {} a -> s {provider = a} :: RulesPackage)

instance Core.FromJSON RulesPackage where
  parseJSON =
    Core.withObject
      "RulesPackage"
      ( \x ->
          RulesPackage'
            Core.<$> (x Core..:? "description")
            Core.<*> (x Core..: "arn")
            Core.<*> (x Core..: "name")
            Core.<*> (x Core..: "version")
            Core.<*> (x Core..: "provider")
      )

instance Core.Hashable RulesPackage

instance Core.NFData RulesPackage
