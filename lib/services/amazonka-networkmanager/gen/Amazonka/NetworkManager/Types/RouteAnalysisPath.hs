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
-- Module      : Amazonka.NetworkManager.Types.RouteAnalysisPath
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.RouteAnalysisPath where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types.PathComponent
import Amazonka.NetworkManager.Types.RouteAnalysisCompletion
import qualified Amazonka.Prelude as Prelude

-- | Describes a route analysis path.
--
-- /See:/ 'newRouteAnalysisPath' smart constructor.
data RouteAnalysisPath = RouteAnalysisPath'
  { -- | The route analysis path.
    path :: Prelude.Maybe [PathComponent],
    -- | The status of the analysis at completion.
    completionStatus :: Prelude.Maybe RouteAnalysisCompletion
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RouteAnalysisPath' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'path', 'routeAnalysisPath_path' - The route analysis path.
--
-- 'completionStatus', 'routeAnalysisPath_completionStatus' - The status of the analysis at completion.
newRouteAnalysisPath ::
  RouteAnalysisPath
newRouteAnalysisPath =
  RouteAnalysisPath'
    { path = Prelude.Nothing,
      completionStatus = Prelude.Nothing
    }

-- | The route analysis path.
routeAnalysisPath_path :: Lens.Lens' RouteAnalysisPath (Prelude.Maybe [PathComponent])
routeAnalysisPath_path = Lens.lens (\RouteAnalysisPath' {path} -> path) (\s@RouteAnalysisPath' {} a -> s {path = a} :: RouteAnalysisPath) Prelude.. Lens.mapping Lens.coerced

-- | The status of the analysis at completion.
routeAnalysisPath_completionStatus :: Lens.Lens' RouteAnalysisPath (Prelude.Maybe RouteAnalysisCompletion)
routeAnalysisPath_completionStatus = Lens.lens (\RouteAnalysisPath' {completionStatus} -> completionStatus) (\s@RouteAnalysisPath' {} a -> s {completionStatus = a} :: RouteAnalysisPath)

instance Data.FromJSON RouteAnalysisPath where
  parseJSON =
    Data.withObject
      "RouteAnalysisPath"
      ( \x ->
          RouteAnalysisPath'
            Prelude.<$> (x Data..:? "Path" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "CompletionStatus")
      )

instance Prelude.Hashable RouteAnalysisPath where
  hashWithSalt _salt RouteAnalysisPath' {..} =
    _salt `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` completionStatus

instance Prelude.NFData RouteAnalysisPath where
  rnf RouteAnalysisPath' {..} =
    Prelude.rnf path
      `Prelude.seq` Prelude.rnf completionStatus
