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
-- Module      : Network.AWS.Rekognition.Types.TechnicalCueType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.TechnicalCueType
  ( TechnicalCueType
      ( ..,
        TechnicalCueType_BlackFrames,
        TechnicalCueType_ColorBars,
        TechnicalCueType_EndCredits
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype TechnicalCueType = TechnicalCueType'
  { fromTechnicalCueType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern TechnicalCueType_BlackFrames :: TechnicalCueType
pattern TechnicalCueType_BlackFrames = TechnicalCueType' "BlackFrames"

pattern TechnicalCueType_ColorBars :: TechnicalCueType
pattern TechnicalCueType_ColorBars = TechnicalCueType' "ColorBars"

pattern TechnicalCueType_EndCredits :: TechnicalCueType
pattern TechnicalCueType_EndCredits = TechnicalCueType' "EndCredits"

{-# COMPLETE
  TechnicalCueType_BlackFrames,
  TechnicalCueType_ColorBars,
  TechnicalCueType_EndCredits,
  TechnicalCueType'
  #-}
