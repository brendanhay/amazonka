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
-- Module      : Amazonka.Detective.Types.UnprocessedGraph
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Detective.Types.UnprocessedGraph where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Behavior graphs that could not be processed in the request.
--
-- /See:/ 'newUnprocessedGraph' smart constructor.
data UnprocessedGraph = UnprocessedGraph'
  { -- | The ARN of the organization behavior graph.
    graphArn :: Prelude.Maybe Prelude.Text,
    -- | The reason data source package information could not be processed for a
    -- behavior graph.
    reason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnprocessedGraph' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'graphArn', 'unprocessedGraph_graphArn' - The ARN of the organization behavior graph.
--
-- 'reason', 'unprocessedGraph_reason' - The reason data source package information could not be processed for a
-- behavior graph.
newUnprocessedGraph ::
  UnprocessedGraph
newUnprocessedGraph =
  UnprocessedGraph'
    { graphArn = Prelude.Nothing,
      reason = Prelude.Nothing
    }

-- | The ARN of the organization behavior graph.
unprocessedGraph_graphArn :: Lens.Lens' UnprocessedGraph (Prelude.Maybe Prelude.Text)
unprocessedGraph_graphArn = Lens.lens (\UnprocessedGraph' {graphArn} -> graphArn) (\s@UnprocessedGraph' {} a -> s {graphArn = a} :: UnprocessedGraph)

-- | The reason data source package information could not be processed for a
-- behavior graph.
unprocessedGraph_reason :: Lens.Lens' UnprocessedGraph (Prelude.Maybe Prelude.Text)
unprocessedGraph_reason = Lens.lens (\UnprocessedGraph' {reason} -> reason) (\s@UnprocessedGraph' {} a -> s {reason = a} :: UnprocessedGraph)

instance Data.FromJSON UnprocessedGraph where
  parseJSON =
    Data.withObject
      "UnprocessedGraph"
      ( \x ->
          UnprocessedGraph'
            Prelude.<$> (x Data..:? "GraphArn")
            Prelude.<*> (x Data..:? "Reason")
      )

instance Prelude.Hashable UnprocessedGraph where
  hashWithSalt _salt UnprocessedGraph' {..} =
    _salt `Prelude.hashWithSalt` graphArn
      `Prelude.hashWithSalt` reason

instance Prelude.NFData UnprocessedGraph where
  rnf UnprocessedGraph' {..} =
    Prelude.rnf graphArn
      `Prelude.seq` Prelude.rnf reason
