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
-- Module      : Amazonka.Panorama.Types.ManifestOverridesPayload
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.ManifestOverridesPayload where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Parameter overrides for an application instance. This is a JSON document
-- that has a single key (@PayloadData@) where the value is an escaped
-- string representation of the overrides document.
--
-- /See:/ 'newManifestOverridesPayload' smart constructor.
data ManifestOverridesPayload = ManifestOverridesPayload'
  { -- | The overrides document.
    payloadData :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ManifestOverridesPayload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'payloadData', 'manifestOverridesPayload_payloadData' - The overrides document.
newManifestOverridesPayload ::
  ManifestOverridesPayload
newManifestOverridesPayload =
  ManifestOverridesPayload'
    { payloadData =
        Prelude.Nothing
    }

-- | The overrides document.
manifestOverridesPayload_payloadData :: Lens.Lens' ManifestOverridesPayload (Prelude.Maybe Prelude.Text)
manifestOverridesPayload_payloadData = Lens.lens (\ManifestOverridesPayload' {payloadData} -> payloadData) (\s@ManifestOverridesPayload' {} a -> s {payloadData = a} :: ManifestOverridesPayload)

instance Core.FromJSON ManifestOverridesPayload where
  parseJSON =
    Core.withObject
      "ManifestOverridesPayload"
      ( \x ->
          ManifestOverridesPayload'
            Prelude.<$> (x Core..:? "PayloadData")
      )

instance Prelude.Hashable ManifestOverridesPayload where
  hashWithSalt _salt ManifestOverridesPayload' {..} =
    _salt `Prelude.hashWithSalt` payloadData

instance Prelude.NFData ManifestOverridesPayload where
  rnf ManifestOverridesPayload' {..} =
    Prelude.rnf payloadData

instance Core.ToJSON ManifestOverridesPayload where
  toJSON ManifestOverridesPayload' {..} =
    Core.object
      ( Prelude.catMaybes
          [("PayloadData" Core..=) Prelude.<$> payloadData]
      )
