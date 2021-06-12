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
-- Module      : Network.AWS.Rekognition.Types.CoversBodyPart
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.CoversBodyPart where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about an item of Personal Protective Equipment covering a
-- corresponding body part. For more information, see
-- DetectProtectiveEquipment.
--
-- /See:/ 'newCoversBodyPart' smart constructor.
data CoversBodyPart = CoversBodyPart'
  { -- | The confidence that Amazon Rekognition has in the value of @Value@.
    confidence :: Core.Maybe Core.Double,
    -- | True if the PPE covers the corresponding body part, otherwise false.
    value :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CoversBodyPart' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'confidence', 'coversBodyPart_confidence' - The confidence that Amazon Rekognition has in the value of @Value@.
--
-- 'value', 'coversBodyPart_value' - True if the PPE covers the corresponding body part, otherwise false.
newCoversBodyPart ::
  CoversBodyPart
newCoversBodyPart =
  CoversBodyPart'
    { confidence = Core.Nothing,
      value = Core.Nothing
    }

-- | The confidence that Amazon Rekognition has in the value of @Value@.
coversBodyPart_confidence :: Lens.Lens' CoversBodyPart (Core.Maybe Core.Double)
coversBodyPart_confidence = Lens.lens (\CoversBodyPart' {confidence} -> confidence) (\s@CoversBodyPart' {} a -> s {confidence = a} :: CoversBodyPart)

-- | True if the PPE covers the corresponding body part, otherwise false.
coversBodyPart_value :: Lens.Lens' CoversBodyPart (Core.Maybe Core.Bool)
coversBodyPart_value = Lens.lens (\CoversBodyPart' {value} -> value) (\s@CoversBodyPart' {} a -> s {value = a} :: CoversBodyPart)

instance Core.FromJSON CoversBodyPart where
  parseJSON =
    Core.withObject
      "CoversBodyPart"
      ( \x ->
          CoversBodyPart'
            Core.<$> (x Core..:? "Confidence")
            Core.<*> (x Core..:? "Value")
      )

instance Core.Hashable CoversBodyPart

instance Core.NFData CoversBodyPart
