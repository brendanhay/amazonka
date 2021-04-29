{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentTier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EnvironmentTier where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the properties of an environment tier
--
-- /See:/ 'newEnvironmentTier' smart constructor.
data EnvironmentTier = EnvironmentTier'
  { -- | The version of this environment tier. When you don\'t set a value to it,
    -- Elastic Beanstalk uses the latest compatible worker tier version.
    --
    -- This member is deprecated. Any specific version that you set may become
    -- out of date. We recommend leaving it unspecified.
    version :: Prelude.Maybe Prelude.Text,
    -- | The name of this environment tier.
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
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentTier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'environmentTier_version' - The version of this environment tier. When you don\'t set a value to it,
-- Elastic Beanstalk uses the latest compatible worker tier version.
--
-- This member is deprecated. Any specific version that you set may become
-- out of date. We recommend leaving it unspecified.
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
newEnvironmentTier ::
  EnvironmentTier
newEnvironmentTier =
  EnvironmentTier'
    { version = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The version of this environment tier. When you don\'t set a value to it,
-- Elastic Beanstalk uses the latest compatible worker tier version.
--
-- This member is deprecated. Any specific version that you set may become
-- out of date. We recommend leaving it unspecified.
environmentTier_version :: Lens.Lens' EnvironmentTier (Prelude.Maybe Prelude.Text)
environmentTier_version = Lens.lens (\EnvironmentTier' {version} -> version) (\s@EnvironmentTier' {} a -> s {version = a} :: EnvironmentTier)

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

instance Prelude.FromXML EnvironmentTier where
  parseXML x =
    EnvironmentTier'
      Prelude.<$> (x Prelude..@? "Version")
      Prelude.<*> (x Prelude..@? "Name")
      Prelude.<*> (x Prelude..@? "Type")

instance Prelude.Hashable EnvironmentTier

instance Prelude.NFData EnvironmentTier

instance Prelude.ToQuery EnvironmentTier where
  toQuery EnvironmentTier' {..} =
    Prelude.mconcat
      [ "Version" Prelude.=: version,
        "Name" Prelude.=: name,
        "Type" Prelude.=: type'
      ]
