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
-- Module      : Amazonka.Rekognition.Types.TechnicalCueType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.TechnicalCueType
  ( TechnicalCueType
      ( ..,
        TechnicalCueType_BlackFrames,
        TechnicalCueType_ColorBars,
        TechnicalCueType_Content,
        TechnicalCueType_EndCredits,
        TechnicalCueType_OpeningCredits,
        TechnicalCueType_Slate,
        TechnicalCueType_StudioLogo
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TechnicalCueType = TechnicalCueType'
  { fromTechnicalCueType ::
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

pattern TechnicalCueType_BlackFrames :: TechnicalCueType
pattern TechnicalCueType_BlackFrames = TechnicalCueType' "BlackFrames"

pattern TechnicalCueType_ColorBars :: TechnicalCueType
pattern TechnicalCueType_ColorBars = TechnicalCueType' "ColorBars"

pattern TechnicalCueType_Content :: TechnicalCueType
pattern TechnicalCueType_Content = TechnicalCueType' "Content"

pattern TechnicalCueType_EndCredits :: TechnicalCueType
pattern TechnicalCueType_EndCredits = TechnicalCueType' "EndCredits"

pattern TechnicalCueType_OpeningCredits :: TechnicalCueType
pattern TechnicalCueType_OpeningCredits = TechnicalCueType' "OpeningCredits"

pattern TechnicalCueType_Slate :: TechnicalCueType
pattern TechnicalCueType_Slate = TechnicalCueType' "Slate"

pattern TechnicalCueType_StudioLogo :: TechnicalCueType
pattern TechnicalCueType_StudioLogo = TechnicalCueType' "StudioLogo"

{-# COMPLETE
  TechnicalCueType_BlackFrames,
  TechnicalCueType_ColorBars,
  TechnicalCueType_Content,
  TechnicalCueType_EndCredits,
  TechnicalCueType_OpeningCredits,
  TechnicalCueType_Slate,
  TechnicalCueType_StudioLogo,
  TechnicalCueType'
  #-}
