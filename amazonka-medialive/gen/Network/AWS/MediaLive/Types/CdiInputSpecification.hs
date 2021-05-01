{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaLive.Types.CdiInputSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.CdiInputSpecification where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.CdiInputResolution
import qualified Network.AWS.Prelude as Prelude

-- | Placeholder documentation for CdiInputSpecification
--
-- /See:/ 'newCdiInputSpecification' smart constructor.
data CdiInputSpecification = CdiInputSpecification'
  { -- | Maximum CDI input resolution
    resolution :: Prelude.Maybe CdiInputResolution
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON CdiInputSpecification where
  parseJSON =
    Prelude.withObject
      "CdiInputSpecification"
      ( \x ->
          CdiInputSpecification'
            Prelude.<$> (x Prelude..:? "resolution")
      )

instance Prelude.Hashable CdiInputSpecification

instance Prelude.NFData CdiInputSpecification

instance Prelude.ToJSON CdiInputSpecification where
  toJSON CdiInputSpecification' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("resolution" Prelude..=) Prelude.<$> resolution]
      )
