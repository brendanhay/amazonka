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
-- Module      : Network.AWS.Rekognition.Types.CelebrityRecognition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.CelebrityRecognition where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.CelebrityDetail

-- | Information about a detected celebrity and the time the celebrity was
-- detected in a stored video. For more information, see
-- GetCelebrityRecognition in the Amazon Rekognition Developer Guide.
--
-- /See:/ 'newCelebrityRecognition' smart constructor.
data CelebrityRecognition = CelebrityRecognition'
  { -- | The time, in milliseconds from the start of the video, that the
    -- celebrity was recognized.
    timestamp :: Prelude.Maybe Prelude.Integer,
    -- | Information about a recognized celebrity.
    celebrity :: Prelude.Maybe CelebrityDetail
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { timestamp = Prelude.Nothing,
      celebrity = Prelude.Nothing
    }

-- | The time, in milliseconds from the start of the video, that the
-- celebrity was recognized.
celebrityRecognition_timestamp :: Lens.Lens' CelebrityRecognition (Prelude.Maybe Prelude.Integer)
celebrityRecognition_timestamp = Lens.lens (\CelebrityRecognition' {timestamp} -> timestamp) (\s@CelebrityRecognition' {} a -> s {timestamp = a} :: CelebrityRecognition)

-- | Information about a recognized celebrity.
celebrityRecognition_celebrity :: Lens.Lens' CelebrityRecognition (Prelude.Maybe CelebrityDetail)
celebrityRecognition_celebrity = Lens.lens (\CelebrityRecognition' {celebrity} -> celebrity) (\s@CelebrityRecognition' {} a -> s {celebrity = a} :: CelebrityRecognition)

instance Prelude.FromJSON CelebrityRecognition where
  parseJSON =
    Prelude.withObject
      "CelebrityRecognition"
      ( \x ->
          CelebrityRecognition'
            Prelude.<$> (x Prelude..:? "Timestamp")
            Prelude.<*> (x Prelude..:? "Celebrity")
      )

instance Prelude.Hashable CelebrityRecognition

instance Prelude.NFData CelebrityRecognition
