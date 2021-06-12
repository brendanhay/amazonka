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
-- Module      : Network.AWS.MediaLive.Types.AacRateControlMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AacRateControlMode
  ( AacRateControlMode
      ( ..,
        AacRateControlMode_CBR,
        AacRateControlMode_VBR
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Aac Rate Control Mode
newtype AacRateControlMode = AacRateControlMode'
  { fromAacRateControlMode ::
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

pattern AacRateControlMode_CBR :: AacRateControlMode
pattern AacRateControlMode_CBR = AacRateControlMode' "CBR"

pattern AacRateControlMode_VBR :: AacRateControlMode
pattern AacRateControlMode_VBR = AacRateControlMode' "VBR"

{-# COMPLETE
  AacRateControlMode_CBR,
  AacRateControlMode_VBR,
  AacRateControlMode'
  #-}
