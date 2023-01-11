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
-- Module      : Amazonka.AccessAnalyzer.Types.Location
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.Location where

import Amazonka.AccessAnalyzer.Types.PathElement
import Amazonka.AccessAnalyzer.Types.Span
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A location in a policy that is represented as a path through the JSON
-- representation and a corresponding span.
--
-- /See:/ 'newLocation' smart constructor.
data Location = Location'
  { -- | A path in a policy, represented as a sequence of path elements.
    path :: [PathElement],
    -- | A span in a policy.
    span :: Span
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Location' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'path', 'location_path' - A path in a policy, represented as a sequence of path elements.
--
-- 'span', 'location_span' - A span in a policy.
newLocation ::
  -- | 'span'
  Span ->
  Location
newLocation pSpan_ =
  Location' {path = Prelude.mempty, span = pSpan_}

-- | A path in a policy, represented as a sequence of path elements.
location_path :: Lens.Lens' Location [PathElement]
location_path = Lens.lens (\Location' {path} -> path) (\s@Location' {} a -> s {path = a} :: Location) Prelude.. Lens.coerced

-- | A span in a policy.
location_span :: Lens.Lens' Location Span
location_span = Lens.lens (\Location' {span} -> span) (\s@Location' {} a -> s {span = a} :: Location)

instance Data.FromJSON Location where
  parseJSON =
    Data.withObject
      "Location"
      ( \x ->
          Location'
            Prelude.<$> (x Data..:? "path" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "span")
      )

instance Prelude.Hashable Location where
  hashWithSalt _salt Location' {..} =
    _salt `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` span

instance Prelude.NFData Location where
  rnf Location' {..} =
    Prelude.rnf path `Prelude.seq` Prelude.rnf span
