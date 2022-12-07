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
-- Module      : Amazonka.GreengrassV2.Types.Component
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.Component where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types.ComponentLatestVersion
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a component.
--
-- /See:/ 'newComponent' smart constructor.
data Component = Component'
  { -- | The name of the component.
    componentName :: Prelude.Maybe Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the component version.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The latest version of the component and its details.
    latestVersion :: Prelude.Maybe ComponentLatestVersion
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Component' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'componentName', 'component_componentName' - The name of the component.
--
-- 'arn', 'component_arn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the component version.
--
-- 'latestVersion', 'component_latestVersion' - The latest version of the component and its details.
newComponent ::
  Component
newComponent =
  Component'
    { componentName = Prelude.Nothing,
      arn = Prelude.Nothing,
      latestVersion = Prelude.Nothing
    }

-- | The name of the component.
component_componentName :: Lens.Lens' Component (Prelude.Maybe Prelude.Text)
component_componentName = Lens.lens (\Component' {componentName} -> componentName) (\s@Component' {} a -> s {componentName = a} :: Component)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the component version.
component_arn :: Lens.Lens' Component (Prelude.Maybe Prelude.Text)
component_arn = Lens.lens (\Component' {arn} -> arn) (\s@Component' {} a -> s {arn = a} :: Component)

-- | The latest version of the component and its details.
component_latestVersion :: Lens.Lens' Component (Prelude.Maybe ComponentLatestVersion)
component_latestVersion = Lens.lens (\Component' {latestVersion} -> latestVersion) (\s@Component' {} a -> s {latestVersion = a} :: Component)

instance Data.FromJSON Component where
  parseJSON =
    Data.withObject
      "Component"
      ( \x ->
          Component'
            Prelude.<$> (x Data..:? "componentName")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "latestVersion")
      )

instance Prelude.Hashable Component where
  hashWithSalt _salt Component' {..} =
    _salt `Prelude.hashWithSalt` componentName
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` latestVersion

instance Prelude.NFData Component where
  rnf Component' {..} =
    Prelude.rnf componentName
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf latestVersion
