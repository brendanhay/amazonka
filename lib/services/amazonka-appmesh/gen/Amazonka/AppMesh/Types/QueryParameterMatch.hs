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
-- Module      : Amazonka.AppMesh.Types.QueryParameterMatch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.QueryParameterMatch where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object representing the query parameter to match.
--
-- /See:/ 'newQueryParameterMatch' smart constructor.
data QueryParameterMatch = QueryParameterMatch'
  { -- | The exact query parameter to match on.
    exact :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryParameterMatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exact', 'queryParameterMatch_exact' - The exact query parameter to match on.
newQueryParameterMatch ::
  QueryParameterMatch
newQueryParameterMatch =
  QueryParameterMatch' {exact = Prelude.Nothing}

-- | The exact query parameter to match on.
queryParameterMatch_exact :: Lens.Lens' QueryParameterMatch (Prelude.Maybe Prelude.Text)
queryParameterMatch_exact = Lens.lens (\QueryParameterMatch' {exact} -> exact) (\s@QueryParameterMatch' {} a -> s {exact = a} :: QueryParameterMatch)

instance Data.FromJSON QueryParameterMatch where
  parseJSON =
    Data.withObject
      "QueryParameterMatch"
      ( \x ->
          QueryParameterMatch'
            Prelude.<$> (x Data..:? "exact")
      )

instance Prelude.Hashable QueryParameterMatch where
  hashWithSalt _salt QueryParameterMatch' {..} =
    _salt `Prelude.hashWithSalt` exact

instance Prelude.NFData QueryParameterMatch where
  rnf QueryParameterMatch' {..} = Prelude.rnf exact

instance Data.ToJSON QueryParameterMatch where
  toJSON QueryParameterMatch' {..} =
    Data.object
      ( Prelude.catMaybes
          [("exact" Data..=) Prelude.<$> exact]
      )
