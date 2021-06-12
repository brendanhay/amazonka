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
-- Module      : Network.AWS.MediaConvert.Types.M2tsAudioBufferModel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M2tsAudioBufferModel
  ( M2tsAudioBufferModel
      ( ..,
        M2tsAudioBufferModel_ATSC,
        M2tsAudioBufferModel_DVB
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Selects between the DVB and ATSC buffer models for Dolby Digital audio.
newtype M2tsAudioBufferModel = M2tsAudioBufferModel'
  { fromM2tsAudioBufferModel ::
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

pattern M2tsAudioBufferModel_ATSC :: M2tsAudioBufferModel
pattern M2tsAudioBufferModel_ATSC = M2tsAudioBufferModel' "ATSC"

pattern M2tsAudioBufferModel_DVB :: M2tsAudioBufferModel
pattern M2tsAudioBufferModel_DVB = M2tsAudioBufferModel' "DVB"

{-# COMPLETE
  M2tsAudioBufferModel_ATSC,
  M2tsAudioBufferModel_DVB,
  M2tsAudioBufferModel'
  #-}
