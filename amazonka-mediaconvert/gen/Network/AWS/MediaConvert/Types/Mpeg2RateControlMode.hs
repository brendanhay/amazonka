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
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2RateControlMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2RateControlMode
  ( Mpeg2RateControlMode
      ( ..,
        Mpeg2RateControlMode_CBR,
        Mpeg2RateControlMode_VBR
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Use Rate control mode (Mpeg2RateControlMode) to specify whether the
-- bitrate is variable (vbr) or constant (cbr).
newtype Mpeg2RateControlMode = Mpeg2RateControlMode'
  { fromMpeg2RateControlMode ::
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

pattern Mpeg2RateControlMode_CBR :: Mpeg2RateControlMode
pattern Mpeg2RateControlMode_CBR = Mpeg2RateControlMode' "CBR"

pattern Mpeg2RateControlMode_VBR :: Mpeg2RateControlMode
pattern Mpeg2RateControlMode_VBR = Mpeg2RateControlMode' "VBR"

{-# COMPLETE
  Mpeg2RateControlMode_CBR,
  Mpeg2RateControlMode_VBR,
  Mpeg2RateControlMode'
  #-}
