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
-- Module      : Amazonka.ECR.Types.LayerFailure
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.LayerFailure where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECR.Types.LayerFailureCode
import qualified Amazonka.Prelude as Prelude

-- | An object representing an Amazon ECR image layer failure.
--
-- /See:/ 'newLayerFailure' smart constructor.
data LayerFailure = LayerFailure'
  { -- | The failure code associated with the failure.
    failureCode :: Prelude.Maybe LayerFailureCode,
    -- | The layer digest associated with the failure.
    layerDigest :: Prelude.Maybe Prelude.Text,
    -- | The reason for the failure.
    failureReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LayerFailure' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureCode', 'layerFailure_failureCode' - The failure code associated with the failure.
--
-- 'layerDigest', 'layerFailure_layerDigest' - The layer digest associated with the failure.
--
-- 'failureReason', 'layerFailure_failureReason' - The reason for the failure.
newLayerFailure ::
  LayerFailure
newLayerFailure =
  LayerFailure'
    { failureCode = Prelude.Nothing,
      layerDigest = Prelude.Nothing,
      failureReason = Prelude.Nothing
    }

-- | The failure code associated with the failure.
layerFailure_failureCode :: Lens.Lens' LayerFailure (Prelude.Maybe LayerFailureCode)
layerFailure_failureCode = Lens.lens (\LayerFailure' {failureCode} -> failureCode) (\s@LayerFailure' {} a -> s {failureCode = a} :: LayerFailure)

-- | The layer digest associated with the failure.
layerFailure_layerDigest :: Lens.Lens' LayerFailure (Prelude.Maybe Prelude.Text)
layerFailure_layerDigest = Lens.lens (\LayerFailure' {layerDigest} -> layerDigest) (\s@LayerFailure' {} a -> s {layerDigest = a} :: LayerFailure)

-- | The reason for the failure.
layerFailure_failureReason :: Lens.Lens' LayerFailure (Prelude.Maybe Prelude.Text)
layerFailure_failureReason = Lens.lens (\LayerFailure' {failureReason} -> failureReason) (\s@LayerFailure' {} a -> s {failureReason = a} :: LayerFailure)

instance Data.FromJSON LayerFailure where
  parseJSON =
    Data.withObject
      "LayerFailure"
      ( \x ->
          LayerFailure'
            Prelude.<$> (x Data..:? "failureCode")
            Prelude.<*> (x Data..:? "layerDigest")
            Prelude.<*> (x Data..:? "failureReason")
      )

instance Prelude.Hashable LayerFailure where
  hashWithSalt _salt LayerFailure' {..} =
    _salt `Prelude.hashWithSalt` failureCode
      `Prelude.hashWithSalt` layerDigest
      `Prelude.hashWithSalt` failureReason

instance Prelude.NFData LayerFailure where
  rnf LayerFailure' {..} =
    Prelude.rnf failureCode
      `Prelude.seq` Prelude.rnf layerDigest
      `Prelude.seq` Prelude.rnf failureReason
