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
-- Module      : Network.AWS.ECR.Types.LayerFailure
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.LayerFailure where

import Network.AWS.ECR.Types.LayerFailureCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing an Amazon ECR image layer failure.
--
-- /See:/ 'newLayerFailure' smart constructor.
data LayerFailure = LayerFailure'
  { -- | The failure code associated with the failure.
    failureCode :: Prelude.Maybe LayerFailureCode,
    -- | The reason for the failure.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The layer digest associated with the failure.
    layerDigest :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'failureReason', 'layerFailure_failureReason' - The reason for the failure.
--
-- 'layerDigest', 'layerFailure_layerDigest' - The layer digest associated with the failure.
newLayerFailure ::
  LayerFailure
newLayerFailure =
  LayerFailure'
    { failureCode = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      layerDigest = Prelude.Nothing
    }

-- | The failure code associated with the failure.
layerFailure_failureCode :: Lens.Lens' LayerFailure (Prelude.Maybe LayerFailureCode)
layerFailure_failureCode = Lens.lens (\LayerFailure' {failureCode} -> failureCode) (\s@LayerFailure' {} a -> s {failureCode = a} :: LayerFailure)

-- | The reason for the failure.
layerFailure_failureReason :: Lens.Lens' LayerFailure (Prelude.Maybe Prelude.Text)
layerFailure_failureReason = Lens.lens (\LayerFailure' {failureReason} -> failureReason) (\s@LayerFailure' {} a -> s {failureReason = a} :: LayerFailure)

-- | The layer digest associated with the failure.
layerFailure_layerDigest :: Lens.Lens' LayerFailure (Prelude.Maybe Prelude.Text)
layerFailure_layerDigest = Lens.lens (\LayerFailure' {layerDigest} -> layerDigest) (\s@LayerFailure' {} a -> s {layerDigest = a} :: LayerFailure)

instance Prelude.FromJSON LayerFailure where
  parseJSON =
    Prelude.withObject
      "LayerFailure"
      ( \x ->
          LayerFailure'
            Prelude.<$> (x Prelude..:? "failureCode")
            Prelude.<*> (x Prelude..:? "failureReason")
            Prelude.<*> (x Prelude..:? "layerDigest")
      )

instance Prelude.Hashable LayerFailure

instance Prelude.NFData LayerFailure
