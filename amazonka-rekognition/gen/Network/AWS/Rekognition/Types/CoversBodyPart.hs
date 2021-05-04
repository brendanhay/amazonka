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
-- Module      : Network.AWS.Rekognition.Types.CoversBodyPart
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.CoversBodyPart where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an item of Personal Protective Equipment covering a
-- corresponding body part. For more information, see
-- DetectProtectiveEquipment.
--
-- /See:/ 'newCoversBodyPart' smart constructor.
data CoversBodyPart = CoversBodyPart'
  { -- | The confidence that Amazon Rekognition has in the value of @Value@.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | True if the PPE covers the corresponding body part, otherwise false.
    value :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { confidence = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The confidence that Amazon Rekognition has in the value of @Value@.
coversBodyPart_confidence :: Lens.Lens' CoversBodyPart (Prelude.Maybe Prelude.Double)
coversBodyPart_confidence = Lens.lens (\CoversBodyPart' {confidence} -> confidence) (\s@CoversBodyPart' {} a -> s {confidence = a} :: CoversBodyPart)

-- | True if the PPE covers the corresponding body part, otherwise false.
coversBodyPart_value :: Lens.Lens' CoversBodyPart (Prelude.Maybe Prelude.Bool)
coversBodyPart_value = Lens.lens (\CoversBodyPart' {value} -> value) (\s@CoversBodyPart' {} a -> s {value = a} :: CoversBodyPart)

instance Prelude.FromJSON CoversBodyPart where
  parseJSON =
    Prelude.withObject
      "CoversBodyPart"
      ( \x ->
          CoversBodyPart'
            Prelude.<$> (x Prelude..:? "Confidence")
            Prelude.<*> (x Prelude..:? "Value")
      )

instance Prelude.Hashable CoversBodyPart

instance Prelude.NFData CoversBodyPart
