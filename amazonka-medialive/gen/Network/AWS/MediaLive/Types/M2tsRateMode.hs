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
-- Module      : Network.AWS.MediaLive.Types.M2tsRateMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsRateMode
  ( M2tsRateMode
      ( ..,
        M2tsRateMode_CBR,
        M2tsRateMode_VBR
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | M2ts Rate Mode
newtype M2tsRateMode = M2tsRateMode'
  { fromM2tsRateMode ::
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

pattern M2tsRateMode_CBR :: M2tsRateMode
pattern M2tsRateMode_CBR = M2tsRateMode' "CBR"

pattern M2tsRateMode_VBR :: M2tsRateMode
pattern M2tsRateMode_VBR = M2tsRateMode' "VBR"

{-# COMPLETE
  M2tsRateMode_CBR,
  M2tsRateMode_VBR,
  M2tsRateMode'
  #-}
