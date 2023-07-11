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
-- Module      : Amazonka.MediaLive.Types.CdiInputSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.CdiInputSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.CdiInputResolution
import qualified Amazonka.Prelude as Prelude

-- | Placeholder documentation for CdiInputSpecification
--
-- /See:/ 'newCdiInputSpecification' smart constructor.
data CdiInputSpecification = CdiInputSpecification'
  { -- | Maximum CDI input resolution
    resolution :: Prelude.Maybe CdiInputResolution
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CdiInputSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolution', 'cdiInputSpecification_resolution' - Maximum CDI input resolution
newCdiInputSpecification ::
  CdiInputSpecification
newCdiInputSpecification =
  CdiInputSpecification'
    { resolution =
        Prelude.Nothing
    }

-- | Maximum CDI input resolution
cdiInputSpecification_resolution :: Lens.Lens' CdiInputSpecification (Prelude.Maybe CdiInputResolution)
cdiInputSpecification_resolution = Lens.lens (\CdiInputSpecification' {resolution} -> resolution) (\s@CdiInputSpecification' {} a -> s {resolution = a} :: CdiInputSpecification)

instance Data.FromJSON CdiInputSpecification where
  parseJSON =
    Data.withObject
      "CdiInputSpecification"
      ( \x ->
          CdiInputSpecification'
            Prelude.<$> (x Data..:? "resolution")
      )

instance Prelude.Hashable CdiInputSpecification where
  hashWithSalt _salt CdiInputSpecification' {..} =
    _salt `Prelude.hashWithSalt` resolution

instance Prelude.NFData CdiInputSpecification where
  rnf CdiInputSpecification' {..} =
    Prelude.rnf resolution

instance Data.ToJSON CdiInputSpecification where
  toJSON CdiInputSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [("resolution" Data..=) Prelude.<$> resolution]
      )
