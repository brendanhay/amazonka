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
-- Module      : Amazonka.ElasticBeanstalk.Types.EnvironmentTier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.EnvironmentTier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the properties of an environment tier
--
-- /See:/ 'newEnvironmentTier' smart constructor.
data EnvironmentTier = EnvironmentTier'
  { -- | The name of this environment tier.
    --
    -- Valid values:
    --
    -- -   For /Web server tier/ – @WebServer@
    --
    -- -   For /Worker tier/ – @Worker@
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of this environment tier.
    --
    -- Valid values:
    --
    -- -   For /Web server tier/ – @Standard@
    --
    -- -   For /Worker tier/ – @SQS\/HTTP@
    type' :: Prelude.Maybe Prelude.Text,
    -- | The version of this environment tier. When you don\'t set a value to it,
    -- Elastic Beanstalk uses the latest compatible worker tier version.
    --
    -- This member is deprecated. Any specific version that you set may become
    -- out of date. We recommend leaving it unspecified.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentTier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'environmentTier_name' - The name of this environment tier.
--
-- Valid values:
--
-- -   For /Web server tier/ – @WebServer@
--
-- -   For /Worker tier/ – @Worker@
--
-- 'type'', 'environmentTier_type' - The type of this environment tier.
--
-- Valid values:
--
-- -   For /Web server tier/ – @Standard@
--
-- -   For /Worker tier/ – @SQS\/HTTP@
--
-- 'version', 'environmentTier_version' - The version of this environment tier. When you don\'t set a value to it,
-- Elastic Beanstalk uses the latest compatible worker tier version.
--
-- This member is deprecated. Any specific version that you set may become
-- out of date. We recommend leaving it unspecified.
newEnvironmentTier ::
  EnvironmentTier
newEnvironmentTier =
  EnvironmentTier'
    { name = Prelude.Nothing,
      type' = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The name of this environment tier.
--
-- Valid values:
--
-- -   For /Web server tier/ – @WebServer@
--
-- -   For /Worker tier/ – @Worker@
environmentTier_name :: Lens.Lens' EnvironmentTier (Prelude.Maybe Prelude.Text)
environmentTier_name = Lens.lens (\EnvironmentTier' {name} -> name) (\s@EnvironmentTier' {} a -> s {name = a} :: EnvironmentTier)

-- | The type of this environment tier.
--
-- Valid values:
--
-- -   For /Web server tier/ – @Standard@
--
-- -   For /Worker tier/ – @SQS\/HTTP@
environmentTier_type :: Lens.Lens' EnvironmentTier (Prelude.Maybe Prelude.Text)
environmentTier_type = Lens.lens (\EnvironmentTier' {type'} -> type') (\s@EnvironmentTier' {} a -> s {type' = a} :: EnvironmentTier)

-- | The version of this environment tier. When you don\'t set a value to it,
-- Elastic Beanstalk uses the latest compatible worker tier version.
--
-- This member is deprecated. Any specific version that you set may become
-- out of date. We recommend leaving it unspecified.
environmentTier_version :: Lens.Lens' EnvironmentTier (Prelude.Maybe Prelude.Text)
environmentTier_version = Lens.lens (\EnvironmentTier' {version} -> version) (\s@EnvironmentTier' {} a -> s {version = a} :: EnvironmentTier)

instance Data.FromXML EnvironmentTier where
  parseXML x =
    EnvironmentTier'
      Prelude.<$> (x Data..@? "Name")
      Prelude.<*> (x Data..@? "Type")
      Prelude.<*> (x Data..@? "Version")

instance Prelude.Hashable EnvironmentTier where
  hashWithSalt _salt EnvironmentTier' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` version

instance Prelude.NFData EnvironmentTier where
  rnf EnvironmentTier' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf type' `Prelude.seq`
        Prelude.rnf version

instance Data.ToQuery EnvironmentTier where
  toQuery EnvironmentTier' {..} =
    Prelude.mconcat
      [ "Name" Data.=: name,
        "Type" Data.=: type',
        "Version" Data.=: version
      ]
