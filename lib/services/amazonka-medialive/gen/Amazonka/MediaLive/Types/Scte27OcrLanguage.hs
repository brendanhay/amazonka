{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaLive.Types.Scte27OcrLanguage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Scte27OcrLanguage
  ( Scte27OcrLanguage
      ( ..,
        Scte27OcrLanguage_DEU,
        Scte27OcrLanguage_ENG,
        Scte27OcrLanguage_FRA,
        Scte27OcrLanguage_NLD,
        Scte27OcrLanguage_POR,
        Scte27OcrLanguage_SPA
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Scte27 Ocr Language
newtype Scte27OcrLanguage = Scte27OcrLanguage'
  { fromScte27OcrLanguage ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern Scte27OcrLanguage_DEU :: Scte27OcrLanguage
pattern Scte27OcrLanguage_DEU = Scte27OcrLanguage' "DEU"

pattern Scte27OcrLanguage_ENG :: Scte27OcrLanguage
pattern Scte27OcrLanguage_ENG = Scte27OcrLanguage' "ENG"

pattern Scte27OcrLanguage_FRA :: Scte27OcrLanguage
pattern Scte27OcrLanguage_FRA = Scte27OcrLanguage' "FRA"

pattern Scte27OcrLanguage_NLD :: Scte27OcrLanguage
pattern Scte27OcrLanguage_NLD = Scte27OcrLanguage' "NLD"

pattern Scte27OcrLanguage_POR :: Scte27OcrLanguage
pattern Scte27OcrLanguage_POR = Scte27OcrLanguage' "POR"

pattern Scte27OcrLanguage_SPA :: Scte27OcrLanguage
pattern Scte27OcrLanguage_SPA = Scte27OcrLanguage' "SPA"

{-# COMPLETE
  Scte27OcrLanguage_DEU,
  Scte27OcrLanguage_ENG,
  Scte27OcrLanguage_FRA,
  Scte27OcrLanguage_NLD,
  Scte27OcrLanguage_POR,
  Scte27OcrLanguage_SPA,
  Scte27OcrLanguage'
  #-}
