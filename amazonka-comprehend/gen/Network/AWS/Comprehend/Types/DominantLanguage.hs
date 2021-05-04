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
-- Module      : Network.AWS.Comprehend.Types.DominantLanguage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DominantLanguage where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    languageCode :: Prelude.Maybe Prelude.Text,
    -- | The level of confidence that Amazon Comprehend has in the accuracy of
    -- the detection.
    score :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { languageCode = Prelude.Nothing,
      score = Prelude.Nothing
    }

-- | The RFC 5646 language code for the dominant language. For more
-- information about RFC 5646, see
-- <https://tools.ietf.org/html/rfc5646 Tags for Identifying Languages> on
-- the /IETF Tools/ web site.
dominantLanguage_languageCode :: Lens.Lens' DominantLanguage (Prelude.Maybe Prelude.Text)
dominantLanguage_languageCode = Lens.lens (\DominantLanguage' {languageCode} -> languageCode) (\s@DominantLanguage' {} a -> s {languageCode = a} :: DominantLanguage)

-- | The level of confidence that Amazon Comprehend has in the accuracy of
-- the detection.
dominantLanguage_score :: Lens.Lens' DominantLanguage (Prelude.Maybe Prelude.Double)
dominantLanguage_score = Lens.lens (\DominantLanguage' {score} -> score) (\s@DominantLanguage' {} a -> s {score = a} :: DominantLanguage)

instance Prelude.FromJSON DominantLanguage where
  parseJSON =
    Prelude.withObject
      "DominantLanguage"
      ( \x ->
          DominantLanguage'
            Prelude.<$> (x Prelude..:? "LanguageCode")
            Prelude.<*> (x Prelude..:? "Score")
      )

instance Prelude.Hashable DominantLanguage

instance Prelude.NFData DominantLanguage
