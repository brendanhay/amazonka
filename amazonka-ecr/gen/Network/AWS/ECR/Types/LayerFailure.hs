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

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types.LayerFailureCode
import qualified Network.AWS.Lens as Lens

-- | An object representing an Amazon ECR image layer failure.
--
-- /See:/ 'newLayerFailure' smart constructor.
data LayerFailure = LayerFailure'
  { -- | The failure code associated with the failure.
    failureCode :: Core.Maybe LayerFailureCode,
    -- | The reason for the failure.
    failureReason :: Core.Maybe Core.Text,
    -- | The layer digest associated with the failure.
    layerDigest :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { failureCode = Core.Nothing,
      failureReason = Core.Nothing,
      layerDigest = Core.Nothing
    }

-- | The failure code associated with the failure.
layerFailure_failureCode :: Lens.Lens' LayerFailure (Core.Maybe LayerFailureCode)
layerFailure_failureCode = Lens.lens (\LayerFailure' {failureCode} -> failureCode) (\s@LayerFailure' {} a -> s {failureCode = a} :: LayerFailure)

-- | The reason for the failure.
layerFailure_failureReason :: Lens.Lens' LayerFailure (Core.Maybe Core.Text)
layerFailure_failureReason = Lens.lens (\LayerFailure' {failureReason} -> failureReason) (\s@LayerFailure' {} a -> s {failureReason = a} :: LayerFailure)

-- | The layer digest associated with the failure.
layerFailure_layerDigest :: Lens.Lens' LayerFailure (Core.Maybe Core.Text)
layerFailure_layerDigest = Lens.lens (\LayerFailure' {layerDigest} -> layerDigest) (\s@LayerFailure' {} a -> s {layerDigest = a} :: LayerFailure)

instance Core.FromJSON LayerFailure where
  parseJSON =
    Core.withObject
      "LayerFailure"
      ( \x ->
          LayerFailure'
            Core.<$> (x Core..:? "failureCode")
            Core.<*> (x Core..:? "failureReason")
            Core.<*> (x Core..:? "layerDigest")
      )

instance Core.Hashable LayerFailure

instance Core.NFData LayerFailure
