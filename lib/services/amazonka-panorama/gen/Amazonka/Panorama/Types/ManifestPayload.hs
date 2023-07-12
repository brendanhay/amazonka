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
-- Module      : Amazonka.Panorama.Types.ManifestPayload
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.ManifestPayload where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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

instance Data.FromJSON ManifestPayload where
  parseJSON =
    Data.withObject
      "ManifestPayload"
      ( \x ->
          ManifestPayload'
            Prelude.<$> (x Data..:? "PayloadData")
      )

instance Prelude.Hashable ManifestPayload where
  hashWithSalt _salt ManifestPayload' {..} =
    _salt `Prelude.hashWithSalt` payloadData

instance Prelude.NFData ManifestPayload where
  rnf ManifestPayload' {..} = Prelude.rnf payloadData

instance Data.ToJSON ManifestPayload where
  toJSON ManifestPayload' {..} =
    Data.object
      ( Prelude.catMaybes
          [("PayloadData" Data..=) Prelude.<$> payloadData]
      )
