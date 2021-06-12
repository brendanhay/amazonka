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
-- Module      : Network.AWS.MediaLive.Types.H264RateControlMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264RateControlMode
  ( H264RateControlMode
      ( ..,
        H264RateControlMode_CBR,
        H264RateControlMode_MULTIPLEX,
        H264RateControlMode_QVBR,
        H264RateControlMode_VBR
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | H264 Rate Control Mode
newtype H264RateControlMode = H264RateControlMode'
  { fromH264RateControlMode ::
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

pattern H264RateControlMode_CBR :: H264RateControlMode
pattern H264RateControlMode_CBR = H264RateControlMode' "CBR"

pattern H264RateControlMode_MULTIPLEX :: H264RateControlMode
pattern H264RateControlMode_MULTIPLEX = H264RateControlMode' "MULTIPLEX"

pattern H264RateControlMode_QVBR :: H264RateControlMode
pattern H264RateControlMode_QVBR = H264RateControlMode' "QVBR"

pattern H264RateControlMode_VBR :: H264RateControlMode
pattern H264RateControlMode_VBR = H264RateControlMode' "VBR"

{-# COMPLETE
  H264RateControlMode_CBR,
  H264RateControlMode_MULTIPLEX,
  H264RateControlMode_QVBR,
  H264RateControlMode_VBR,
  H264RateControlMode'
  #-}
