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
-- Module      : Amazonka.Rekognition.Types.ConnectedHomeSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.ConnectedHomeSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Label detection settings to use on a streaming video. Defining the
-- settings is required in the request parameter for CreateStreamProcessor.
-- Including this setting in the @CreateStreamProcessor@ request enables
-- you to use the stream processor for label detection. You can then select
-- what you want the stream processor to detect, such as people or pets.
-- When the stream processor has started, one notification is sent for each
-- object class specified. For example, if packages and pets are selected,
-- one SNS notification is published the first time a package is detected
-- and one SNS notification is published the first time a pet is detected,
-- as well as an end-of-session summary.
--
-- /See:/ 'newConnectedHomeSettings' smart constructor.
data ConnectedHomeSettings = ConnectedHomeSettings'
  { -- | The minimum confidence required to label an object in the video.
    minConfidence :: Prelude.Maybe Prelude.Double,
    -- | Specifies what you want to detect in the video, such as people,
    -- packages, or pets. The current valid labels you can include in this list
    -- are: \"PERSON\", \"PET\", \"PACKAGE\", and \"ALL\".
    labels :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectedHomeSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minConfidence', 'connectedHomeSettings_minConfidence' - The minimum confidence required to label an object in the video.
--
-- 'labels', 'connectedHomeSettings_labels' - Specifies what you want to detect in the video, such as people,
-- packages, or pets. The current valid labels you can include in this list
-- are: \"PERSON\", \"PET\", \"PACKAGE\", and \"ALL\".
newConnectedHomeSettings ::
  -- | 'labels'
  Prelude.NonEmpty Prelude.Text ->
  ConnectedHomeSettings
newConnectedHomeSettings pLabels_ =
  ConnectedHomeSettings'
    { minConfidence =
        Prelude.Nothing,
      labels = Lens.coerced Lens.# pLabels_
    }

-- | The minimum confidence required to label an object in the video.
connectedHomeSettings_minConfidence :: Lens.Lens' ConnectedHomeSettings (Prelude.Maybe Prelude.Double)
connectedHomeSettings_minConfidence = Lens.lens (\ConnectedHomeSettings' {minConfidence} -> minConfidence) (\s@ConnectedHomeSettings' {} a -> s {minConfidence = a} :: ConnectedHomeSettings)

-- | Specifies what you want to detect in the video, such as people,
-- packages, or pets. The current valid labels you can include in this list
-- are: \"PERSON\", \"PET\", \"PACKAGE\", and \"ALL\".
connectedHomeSettings_labels :: Lens.Lens' ConnectedHomeSettings (Prelude.NonEmpty Prelude.Text)
connectedHomeSettings_labels = Lens.lens (\ConnectedHomeSettings' {labels} -> labels) (\s@ConnectedHomeSettings' {} a -> s {labels = a} :: ConnectedHomeSettings) Prelude.. Lens.coerced

instance Data.FromJSON ConnectedHomeSettings where
  parseJSON =
    Data.withObject
      "ConnectedHomeSettings"
      ( \x ->
          ConnectedHomeSettings'
            Prelude.<$> (x Data..:? "MinConfidence")
            Prelude.<*> (x Data..: "Labels")
      )

instance Prelude.Hashable ConnectedHomeSettings where
  hashWithSalt _salt ConnectedHomeSettings' {..} =
    _salt
      `Prelude.hashWithSalt` minConfidence
      `Prelude.hashWithSalt` labels

instance Prelude.NFData ConnectedHomeSettings where
  rnf ConnectedHomeSettings' {..} =
    Prelude.rnf minConfidence
      `Prelude.seq` Prelude.rnf labels

instance Data.ToJSON ConnectedHomeSettings where
  toJSON ConnectedHomeSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MinConfidence" Data..=) Prelude.<$> minConfidence,
            Prelude.Just ("Labels" Data..= labels)
          ]
      )
