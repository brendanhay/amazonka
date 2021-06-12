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
-- Module      : Network.AWS.Comprehend.Types.DominantLanguage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DominantLanguage where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Returns the code for the dominant language in the input text and the
-- level of confidence that Amazon Comprehend has in the accuracy of the
-- detection.
--
-- /See:/ 'newDominantLanguage' smart constructor.
data DominantLanguage = DominantLanguage'
  { -- | The RFC 5646 language code for the dominant language. For more
    -- information about RFC 5646, see
    -- <https://tools.ietf.org/html/rfc5646 Tags for Identifying Languages> on
    -- the /IETF Tools/ web site.
    languageCode :: Core.Maybe Core.Text,
    -- | The level of confidence that Amazon Comprehend has in the accuracy of
    -- the detection.
    score :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DominantLanguage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'dominantLanguage_languageCode' - The RFC 5646 language code for the dominant language. For more
-- information about RFC 5646, see
-- <https://tools.ietf.org/html/rfc5646 Tags for Identifying Languages> on
-- the /IETF Tools/ web site.
--
-- 'score', 'dominantLanguage_score' - The level of confidence that Amazon Comprehend has in the accuracy of
-- the detection.
newDominantLanguage ::
  DominantLanguage
newDominantLanguage =
  DominantLanguage'
    { languageCode = Core.Nothing,
      score = Core.Nothing
    }

-- | The RFC 5646 language code for the dominant language. For more
-- information about RFC 5646, see
-- <https://tools.ietf.org/html/rfc5646 Tags for Identifying Languages> on
-- the /IETF Tools/ web site.
dominantLanguage_languageCode :: Lens.Lens' DominantLanguage (Core.Maybe Core.Text)
dominantLanguage_languageCode = Lens.lens (\DominantLanguage' {languageCode} -> languageCode) (\s@DominantLanguage' {} a -> s {languageCode = a} :: DominantLanguage)

-- | The level of confidence that Amazon Comprehend has in the accuracy of
-- the detection.
dominantLanguage_score :: Lens.Lens' DominantLanguage (Core.Maybe Core.Double)
dominantLanguage_score = Lens.lens (\DominantLanguage' {score} -> score) (\s@DominantLanguage' {} a -> s {score = a} :: DominantLanguage)

instance Core.FromJSON DominantLanguage where
  parseJSON =
    Core.withObject
      "DominantLanguage"
      ( \x ->
          DominantLanguage'
            Core.<$> (x Core..:? "LanguageCode")
            Core.<*> (x Core..:? "Score")
      )

instance Core.Hashable DominantLanguage

instance Core.NFData DominantLanguage
