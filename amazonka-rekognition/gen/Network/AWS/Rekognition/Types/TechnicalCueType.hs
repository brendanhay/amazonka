{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype TechnicalCueType = TechnicalCueType'
  { fromTechnicalCueType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
