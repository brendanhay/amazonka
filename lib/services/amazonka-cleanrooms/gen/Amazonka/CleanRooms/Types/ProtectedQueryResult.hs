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
-- Module      : Amazonka.CleanRooms.Types.ProtectedQueryResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.ProtectedQueryResult where

import Amazonka.CleanRooms.Types.ProtectedQueryOutput
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about the query results.
--
-- /See:/ 'newProtectedQueryResult' smart constructor.
data ProtectedQueryResult = ProtectedQueryResult'
  { -- | The output of the protected query.
    output :: ProtectedQueryOutput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProtectedQueryResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'output', 'protectedQueryResult_output' - The output of the protected query.
newProtectedQueryResult ::
  -- | 'output'
  ProtectedQueryOutput ->
  ProtectedQueryResult
newProtectedQueryResult pOutput_ =
  ProtectedQueryResult' {output = pOutput_}

-- | The output of the protected query.
protectedQueryResult_output :: Lens.Lens' ProtectedQueryResult ProtectedQueryOutput
protectedQueryResult_output = Lens.lens (\ProtectedQueryResult' {output} -> output) (\s@ProtectedQueryResult' {} a -> s {output = a} :: ProtectedQueryResult)

instance Data.FromJSON ProtectedQueryResult where
  parseJSON =
    Data.withObject
      "ProtectedQueryResult"
      ( \x ->
          ProtectedQueryResult'
            Prelude.<$> (x Data..: "output")
      )

instance Prelude.Hashable ProtectedQueryResult where
  hashWithSalt _salt ProtectedQueryResult' {..} =
    _salt `Prelude.hashWithSalt` output

instance Prelude.NFData ProtectedQueryResult where
  rnf ProtectedQueryResult' {..} = Prelude.rnf output
