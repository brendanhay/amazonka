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
-- Module      : Network.AWS.Rekognition.Types.CelebrityRecognition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.CelebrityRecognition where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types.CelebrityDetail

-- | Information about a detected celebrity and the time the celebrity was
-- detected in a stored video. For more information, see
-- GetCelebrityRecognition in the Amazon Rekognition Developer Guide.
--
-- /See:/ 'newCelebrityRecognition' smart constructor.
data CelebrityRecognition = CelebrityRecognition'
  { -- | The time, in milliseconds from the start of the video, that the
    -- celebrity was recognized.
    timestamp :: Core.Maybe Core.Integer,
    -- | Information about a recognized celebrity.
    celebrity :: Core.Maybe CelebrityDetail
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CelebrityRecognition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timestamp', 'celebrityRecognition_timestamp' - The time, in milliseconds from the start of the video, that the
-- celebrity was recognized.
--
-- 'celebrity', 'celebrityRecognition_celebrity' - Information about a recognized celebrity.
newCelebrityRecognition ::
  CelebrityRecognition
newCelebrityRecognition =
  CelebrityRecognition'
    { timestamp = Core.Nothing,
      celebrity = Core.Nothing
    }

-- | The time, in milliseconds from the start of the video, that the
-- celebrity was recognized.
celebrityRecognition_timestamp :: Lens.Lens' CelebrityRecognition (Core.Maybe Core.Integer)
celebrityRecognition_timestamp = Lens.lens (\CelebrityRecognition' {timestamp} -> timestamp) (\s@CelebrityRecognition' {} a -> s {timestamp = a} :: CelebrityRecognition)

-- | Information about a recognized celebrity.
celebrityRecognition_celebrity :: Lens.Lens' CelebrityRecognition (Core.Maybe CelebrityDetail)
celebrityRecognition_celebrity = Lens.lens (\CelebrityRecognition' {celebrity} -> celebrity) (\s@CelebrityRecognition' {} a -> s {celebrity = a} :: CelebrityRecognition)

instance Core.FromJSON CelebrityRecognition where
  parseJSON =
    Core.withObject
      "CelebrityRecognition"
      ( \x ->
          CelebrityRecognition'
            Core.<$> (x Core..:? "Timestamp")
            Core.<*> (x Core..:? "Celebrity")
      )

instance Core.Hashable CelebrityRecognition

instance Core.NFData CelebrityRecognition
