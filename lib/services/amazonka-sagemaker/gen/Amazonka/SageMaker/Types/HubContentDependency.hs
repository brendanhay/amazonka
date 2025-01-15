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
-- Module      : Amazonka.SageMaker.Types.HubContentDependency
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.HubContentDependency where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Any dependencies related to hub content, such as scripts, model
-- artifacts, datasets, or notebooks.
--
-- /See:/ 'newHubContentDependency' smart constructor.
data HubContentDependency = HubContentDependency'
  { -- | The hub content dependency copy path.
    dependencyCopyPath :: Prelude.Maybe Prelude.Text,
    -- | The hub content dependency origin path.
    dependencyOriginPath :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HubContentDependency' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dependencyCopyPath', 'hubContentDependency_dependencyCopyPath' - The hub content dependency copy path.
--
-- 'dependencyOriginPath', 'hubContentDependency_dependencyOriginPath' - The hub content dependency origin path.
newHubContentDependency ::
  HubContentDependency
newHubContentDependency =
  HubContentDependency'
    { dependencyCopyPath =
        Prelude.Nothing,
      dependencyOriginPath = Prelude.Nothing
    }

-- | The hub content dependency copy path.
hubContentDependency_dependencyCopyPath :: Lens.Lens' HubContentDependency (Prelude.Maybe Prelude.Text)
hubContentDependency_dependencyCopyPath = Lens.lens (\HubContentDependency' {dependencyCopyPath} -> dependencyCopyPath) (\s@HubContentDependency' {} a -> s {dependencyCopyPath = a} :: HubContentDependency)

-- | The hub content dependency origin path.
hubContentDependency_dependencyOriginPath :: Lens.Lens' HubContentDependency (Prelude.Maybe Prelude.Text)
hubContentDependency_dependencyOriginPath = Lens.lens (\HubContentDependency' {dependencyOriginPath} -> dependencyOriginPath) (\s@HubContentDependency' {} a -> s {dependencyOriginPath = a} :: HubContentDependency)

instance Data.FromJSON HubContentDependency where
  parseJSON =
    Data.withObject
      "HubContentDependency"
      ( \x ->
          HubContentDependency'
            Prelude.<$> (x Data..:? "DependencyCopyPath")
            Prelude.<*> (x Data..:? "DependencyOriginPath")
      )

instance Prelude.Hashable HubContentDependency where
  hashWithSalt _salt HubContentDependency' {..} =
    _salt
      `Prelude.hashWithSalt` dependencyCopyPath
      `Prelude.hashWithSalt` dependencyOriginPath

instance Prelude.NFData HubContentDependency where
  rnf HubContentDependency' {..} =
    Prelude.rnf dependencyCopyPath `Prelude.seq`
      Prelude.rnf dependencyOriginPath
