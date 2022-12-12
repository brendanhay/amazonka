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
-- Module      : Amazonka.RolesAnywhere.Types.Source
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RolesAnywhere.Types.Source where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RolesAnywhere.Types.SourceData
import Amazonka.RolesAnywhere.Types.TrustAnchorType

-- | The trust anchor type and its related certificate data.
--
-- /See:/ 'newSource' smart constructor.
data Source = Source'
  { -- | The data field of the trust anchor depending on its type.
    sourceData :: Prelude.Maybe SourceData,
    -- | The type of the trust anchor.
    sourceType :: Prelude.Maybe TrustAnchorType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Source' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceData', 'source_sourceData' - The data field of the trust anchor depending on its type.
--
-- 'sourceType', 'source_sourceType' - The type of the trust anchor.
newSource ::
  Source
newSource =
  Source'
    { sourceData = Prelude.Nothing,
      sourceType = Prelude.Nothing
    }

-- | The data field of the trust anchor depending on its type.
source_sourceData :: Lens.Lens' Source (Prelude.Maybe SourceData)
source_sourceData = Lens.lens (\Source' {sourceData} -> sourceData) (\s@Source' {} a -> s {sourceData = a} :: Source)

-- | The type of the trust anchor.
source_sourceType :: Lens.Lens' Source (Prelude.Maybe TrustAnchorType)
source_sourceType = Lens.lens (\Source' {sourceType} -> sourceType) (\s@Source' {} a -> s {sourceType = a} :: Source)

instance Data.FromJSON Source where
  parseJSON =
    Data.withObject
      "Source"
      ( \x ->
          Source'
            Prelude.<$> (x Data..:? "sourceData")
            Prelude.<*> (x Data..:? "sourceType")
      )

instance Prelude.Hashable Source where
  hashWithSalt _salt Source' {..} =
    _salt `Prelude.hashWithSalt` sourceData
      `Prelude.hashWithSalt` sourceType

instance Prelude.NFData Source where
  rnf Source' {..} =
    Prelude.rnf sourceData
      `Prelude.seq` Prelude.rnf sourceType

instance Data.ToJSON Source where
  toJSON Source' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("sourceData" Data..=) Prelude.<$> sourceData,
            ("sourceType" Data..=) Prelude.<$> sourceType
          ]
      )
