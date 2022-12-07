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
-- Module      : Amazonka.EC2.Types.NetworkInsightsAccessScopeContent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.NetworkInsightsAccessScopeContent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.AccessScopePath
import qualified Amazonka.Prelude as Prelude

-- | Describes the Network Access Scope content.
--
-- /See:/ 'newNetworkInsightsAccessScopeContent' smart constructor.
data NetworkInsightsAccessScopeContent = NetworkInsightsAccessScopeContent'
  { -- | The ID of the Network Access Scope.
    networkInsightsAccessScopeId :: Prelude.Maybe Prelude.Text,
    -- | The paths to exclude.
    excludePaths :: Prelude.Maybe [AccessScopePath],
    -- | The paths to match.
    matchPaths :: Prelude.Maybe [AccessScopePath]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkInsightsAccessScopeContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkInsightsAccessScopeId', 'networkInsightsAccessScopeContent_networkInsightsAccessScopeId' - The ID of the Network Access Scope.
--
-- 'excludePaths', 'networkInsightsAccessScopeContent_excludePaths' - The paths to exclude.
--
-- 'matchPaths', 'networkInsightsAccessScopeContent_matchPaths' - The paths to match.
newNetworkInsightsAccessScopeContent ::
  NetworkInsightsAccessScopeContent
newNetworkInsightsAccessScopeContent =
  NetworkInsightsAccessScopeContent'
    { networkInsightsAccessScopeId =
        Prelude.Nothing,
      excludePaths = Prelude.Nothing,
      matchPaths = Prelude.Nothing
    }

-- | The ID of the Network Access Scope.
networkInsightsAccessScopeContent_networkInsightsAccessScopeId :: Lens.Lens' NetworkInsightsAccessScopeContent (Prelude.Maybe Prelude.Text)
networkInsightsAccessScopeContent_networkInsightsAccessScopeId = Lens.lens (\NetworkInsightsAccessScopeContent' {networkInsightsAccessScopeId} -> networkInsightsAccessScopeId) (\s@NetworkInsightsAccessScopeContent' {} a -> s {networkInsightsAccessScopeId = a} :: NetworkInsightsAccessScopeContent)

-- | The paths to exclude.
networkInsightsAccessScopeContent_excludePaths :: Lens.Lens' NetworkInsightsAccessScopeContent (Prelude.Maybe [AccessScopePath])
networkInsightsAccessScopeContent_excludePaths = Lens.lens (\NetworkInsightsAccessScopeContent' {excludePaths} -> excludePaths) (\s@NetworkInsightsAccessScopeContent' {} a -> s {excludePaths = a} :: NetworkInsightsAccessScopeContent) Prelude.. Lens.mapping Lens.coerced

-- | The paths to match.
networkInsightsAccessScopeContent_matchPaths :: Lens.Lens' NetworkInsightsAccessScopeContent (Prelude.Maybe [AccessScopePath])
networkInsightsAccessScopeContent_matchPaths = Lens.lens (\NetworkInsightsAccessScopeContent' {matchPaths} -> matchPaths) (\s@NetworkInsightsAccessScopeContent' {} a -> s {matchPaths = a} :: NetworkInsightsAccessScopeContent) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromXML
    NetworkInsightsAccessScopeContent
  where
  parseXML x =
    NetworkInsightsAccessScopeContent'
      Prelude.<$> (x Data..@? "networkInsightsAccessScopeId")
      Prelude.<*> ( x Data..@? "excludePathSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "matchPathSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )

instance
  Prelude.Hashable
    NetworkInsightsAccessScopeContent
  where
  hashWithSalt
    _salt
    NetworkInsightsAccessScopeContent' {..} =
      _salt
        `Prelude.hashWithSalt` networkInsightsAccessScopeId
        `Prelude.hashWithSalt` excludePaths
        `Prelude.hashWithSalt` matchPaths

instance
  Prelude.NFData
    NetworkInsightsAccessScopeContent
  where
  rnf NetworkInsightsAccessScopeContent' {..} =
    Prelude.rnf networkInsightsAccessScopeId
      `Prelude.seq` Prelude.rnf excludePaths
      `Prelude.seq` Prelude.rnf matchPaths
