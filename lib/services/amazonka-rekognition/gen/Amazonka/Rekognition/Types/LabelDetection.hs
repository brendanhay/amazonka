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
-- Module      : Amazonka.Rekognition.Types.LabelDetection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.LabelDetection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.Label

-- | Information about a label detected in a video analysis request and the
-- time the label was detected in the video.
--
-- /See:/ 'newLabelDetection' smart constructor.
data LabelDetection = LabelDetection'
  { -- | Details about the detected label.
    label :: Prelude.Maybe Label,
    -- | Time, in milliseconds from the start of the video, that the label was
    -- detected. Note that @Timestamp@ is not guaranteed to be accurate to the
    -- individual frame where the label first appears.
    timestamp :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LabelDetection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'label', 'labelDetection_label' - Details about the detected label.
--
-- 'timestamp', 'labelDetection_timestamp' - Time, in milliseconds from the start of the video, that the label was
-- detected. Note that @Timestamp@ is not guaranteed to be accurate to the
-- individual frame where the label first appears.
newLabelDetection ::
  LabelDetection
newLabelDetection =
  LabelDetection'
    { label = Prelude.Nothing,
      timestamp = Prelude.Nothing
    }

-- | Details about the detected label.
labelDetection_label :: Lens.Lens' LabelDetection (Prelude.Maybe Label)
labelDetection_label = Lens.lens (\LabelDetection' {label} -> label) (\s@LabelDetection' {} a -> s {label = a} :: LabelDetection)

-- | Time, in milliseconds from the start of the video, that the label was
-- detected. Note that @Timestamp@ is not guaranteed to be accurate to the
-- individual frame where the label first appears.
labelDetection_timestamp :: Lens.Lens' LabelDetection (Prelude.Maybe Prelude.Integer)
labelDetection_timestamp = Lens.lens (\LabelDetection' {timestamp} -> timestamp) (\s@LabelDetection' {} a -> s {timestamp = a} :: LabelDetection)

instance Data.FromJSON LabelDetection where
  parseJSON =
    Data.withObject
      "LabelDetection"
      ( \x ->
          LabelDetection'
            Prelude.<$> (x Data..:? "Label")
            Prelude.<*> (x Data..:? "Timestamp")
      )

instance Prelude.Hashable LabelDetection where
  hashWithSalt _salt LabelDetection' {..} =
    _salt `Prelude.hashWithSalt` label
      `Prelude.hashWithSalt` timestamp

instance Prelude.NFData LabelDetection where
  rnf LabelDetection' {..} =
    Prelude.rnf label
      `Prelude.seq` Prelude.rnf timestamp
