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
-- Module      : Amazonka.Rekognition.Types.LabelDetectionSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.LabelDetectionSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.GeneralLabelsSettings

-- | Contains the specified filters that should be applied to a list of
-- returned GENERAL_LABELS.
--
-- /See:/ 'newLabelDetectionSettings' smart constructor.
data LabelDetectionSettings = LabelDetectionSettings'
  { generalLabels :: Prelude.Maybe GeneralLabelsSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LabelDetectionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'generalLabels', 'labelDetectionSettings_generalLabels' - Undocumented member.
newLabelDetectionSettings ::
  LabelDetectionSettings
newLabelDetectionSettings =
  LabelDetectionSettings'
    { generalLabels =
        Prelude.Nothing
    }

-- | Undocumented member.
labelDetectionSettings_generalLabels :: Lens.Lens' LabelDetectionSettings (Prelude.Maybe GeneralLabelsSettings)
labelDetectionSettings_generalLabels = Lens.lens (\LabelDetectionSettings' {generalLabels} -> generalLabels) (\s@LabelDetectionSettings' {} a -> s {generalLabels = a} :: LabelDetectionSettings)

instance Prelude.Hashable LabelDetectionSettings where
  hashWithSalt _salt LabelDetectionSettings' {..} =
    _salt `Prelude.hashWithSalt` generalLabels

instance Prelude.NFData LabelDetectionSettings where
  rnf LabelDetectionSettings' {..} =
    Prelude.rnf generalLabels

instance Data.ToJSON LabelDetectionSettings where
  toJSON LabelDetectionSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GeneralLabels" Data..=)
              Prelude.<$> generalLabels
          ]
      )
