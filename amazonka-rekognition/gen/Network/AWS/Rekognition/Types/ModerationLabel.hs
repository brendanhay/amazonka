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
-- Module      : Network.AWS.Rekognition.Types.ModerationLabel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ModerationLabel where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about a single type of unsafe content found in an
-- image or video. Each type of moderated content has a label within a
-- hierarchical taxonomy. For more information, see Detecting Unsafe
-- Content in the Amazon Rekognition Developer Guide.
--
-- /See:/ 'newModerationLabel' smart constructor.
data ModerationLabel = ModerationLabel'
  { -- | The label name for the type of unsafe content detected in the image.
    name :: Prelude.Maybe Prelude.Text,
    -- | Specifies the confidence that Amazon Rekognition has that the label has
    -- been correctly identified.
    --
    -- If you don\'t specify the @MinConfidence@ parameter in the call to
    -- @DetectModerationLabels@, the operation returns labels with a confidence
    -- value greater than or equal to 50 percent.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | The name for the parent label. Labels at the top level of the hierarchy
    -- have the parent label @\"\"@.
    parentName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModerationLabel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'moderationLabel_name' - The label name for the type of unsafe content detected in the image.
--
-- 'confidence', 'moderationLabel_confidence' - Specifies the confidence that Amazon Rekognition has that the label has
-- been correctly identified.
--
-- If you don\'t specify the @MinConfidence@ parameter in the call to
-- @DetectModerationLabels@, the operation returns labels with a confidence
-- value greater than or equal to 50 percent.
--
-- 'parentName', 'moderationLabel_parentName' - The name for the parent label. Labels at the top level of the hierarchy
-- have the parent label @\"\"@.
newModerationLabel ::
  ModerationLabel
newModerationLabel =
  ModerationLabel'
    { name = Prelude.Nothing,
      confidence = Prelude.Nothing,
      parentName = Prelude.Nothing
    }

-- | The label name for the type of unsafe content detected in the image.
moderationLabel_name :: Lens.Lens' ModerationLabel (Prelude.Maybe Prelude.Text)
moderationLabel_name = Lens.lens (\ModerationLabel' {name} -> name) (\s@ModerationLabel' {} a -> s {name = a} :: ModerationLabel)

-- | Specifies the confidence that Amazon Rekognition has that the label has
-- been correctly identified.
--
-- If you don\'t specify the @MinConfidence@ parameter in the call to
-- @DetectModerationLabels@, the operation returns labels with a confidence
-- value greater than or equal to 50 percent.
moderationLabel_confidence :: Lens.Lens' ModerationLabel (Prelude.Maybe Prelude.Double)
moderationLabel_confidence = Lens.lens (\ModerationLabel' {confidence} -> confidence) (\s@ModerationLabel' {} a -> s {confidence = a} :: ModerationLabel)

-- | The name for the parent label. Labels at the top level of the hierarchy
-- have the parent label @\"\"@.
moderationLabel_parentName :: Lens.Lens' ModerationLabel (Prelude.Maybe Prelude.Text)
moderationLabel_parentName = Lens.lens (\ModerationLabel' {parentName} -> parentName) (\s@ModerationLabel' {} a -> s {parentName = a} :: ModerationLabel)

instance Prelude.FromJSON ModerationLabel where
  parseJSON =
    Prelude.withObject
      "ModerationLabel"
      ( \x ->
          ModerationLabel'
            Prelude.<$> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Confidence")
            Prelude.<*> (x Prelude..:? "ParentName")
      )

instance Prelude.Hashable ModerationLabel

instance Prelude.NFData ModerationLabel
