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
-- Module      : Amazonka.ServiceCatalog.Types.SourceConnectionParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.SourceConnectionParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceCatalog.Types.CodeStarParameters

-- | Provides connection details.
--
-- /See:/ 'newSourceConnectionParameters' smart constructor.
data SourceConnectionParameters = SourceConnectionParameters'
  { -- | Provides @ConnectionType@ details.
    codeStar :: Prelude.Maybe CodeStarParameters
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceConnectionParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeStar', 'sourceConnectionParameters_codeStar' - Provides @ConnectionType@ details.
newSourceConnectionParameters ::
  SourceConnectionParameters
newSourceConnectionParameters =
  SourceConnectionParameters'
    { codeStar =
        Prelude.Nothing
    }

-- | Provides @ConnectionType@ details.
sourceConnectionParameters_codeStar :: Lens.Lens' SourceConnectionParameters (Prelude.Maybe CodeStarParameters)
sourceConnectionParameters_codeStar = Lens.lens (\SourceConnectionParameters' {codeStar} -> codeStar) (\s@SourceConnectionParameters' {} a -> s {codeStar = a} :: SourceConnectionParameters)

instance Core.FromJSON SourceConnectionParameters where
  parseJSON =
    Core.withObject
      "SourceConnectionParameters"
      ( \x ->
          SourceConnectionParameters'
            Prelude.<$> (x Core..:? "CodeStar")
      )

instance Prelude.Hashable SourceConnectionParameters where
  hashWithSalt _salt SourceConnectionParameters' {..} =
    _salt `Prelude.hashWithSalt` codeStar

instance Prelude.NFData SourceConnectionParameters where
  rnf SourceConnectionParameters' {..} =
    Prelude.rnf codeStar

instance Core.ToJSON SourceConnectionParameters where
  toJSON SourceConnectionParameters' {..} =
    Core.object
      ( Prelude.catMaybes
          [("CodeStar" Core..=) Prelude.<$> codeStar]
      )
