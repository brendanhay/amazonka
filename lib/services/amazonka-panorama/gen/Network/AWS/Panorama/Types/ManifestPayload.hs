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
-- Module      : Network.AWS.Panorama.Types.ManifestPayload
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Panorama.Types.ManifestPayload where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A application verion\'s manifest file. This is a JSON document that has
-- a single key (@PayloadData@) where the value is an escaped string
-- representation of the application manifest (@graph.json@). This file is
-- located in the @graphs@ folder in your application source.
--
-- /See:/ 'newManifestPayload' smart constructor.
data ManifestPayload = ManifestPayload'
  { -- | The application manifest.
    payloadData :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ManifestPayload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'payloadData', 'manifestPayload_payloadData' - The application manifest.
newManifestPayload ::
  ManifestPayload
newManifestPayload =
  ManifestPayload' {payloadData = Prelude.Nothing}

-- | The application manifest.
manifestPayload_payloadData :: Lens.Lens' ManifestPayload (Prelude.Maybe Prelude.Text)
manifestPayload_payloadData = Lens.lens (\ManifestPayload' {payloadData} -> payloadData) (\s@ManifestPayload' {} a -> s {payloadData = a} :: ManifestPayload)

instance Core.FromJSON ManifestPayload where
  parseJSON =
    Core.withObject
      "ManifestPayload"
      ( \x ->
          ManifestPayload'
            Prelude.<$> (x Core..:? "PayloadData")
      )

instance Prelude.Hashable ManifestPayload

instance Prelude.NFData ManifestPayload

instance Core.ToJSON ManifestPayload where
  toJSON ManifestPayload' {..} =
    Core.object
      ( Prelude.catMaybes
          [("PayloadData" Core..=) Prelude.<$> payloadData]
      )
