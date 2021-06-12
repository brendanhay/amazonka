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
-- Module      : Network.AWS.MediaLive.Types.H264Profile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264Profile
  ( H264Profile
      ( ..,
        H264Profile_BASELINE,
        H264Profile_HIGH,
        H264Profile_HIGH_10BIT,
        H264Profile_HIGH_422,
        H264Profile_HIGH_422_10BIT,
        H264Profile_MAIN
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | H264 Profile
newtype H264Profile = H264Profile'
  { fromH264Profile ::
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

pattern H264Profile_BASELINE :: H264Profile
pattern H264Profile_BASELINE = H264Profile' "BASELINE"

pattern H264Profile_HIGH :: H264Profile
pattern H264Profile_HIGH = H264Profile' "HIGH"

pattern H264Profile_HIGH_10BIT :: H264Profile
pattern H264Profile_HIGH_10BIT = H264Profile' "HIGH_10BIT"

pattern H264Profile_HIGH_422 :: H264Profile
pattern H264Profile_HIGH_422 = H264Profile' "HIGH_422"

pattern H264Profile_HIGH_422_10BIT :: H264Profile
pattern H264Profile_HIGH_422_10BIT = H264Profile' "HIGH_422_10BIT"

pattern H264Profile_MAIN :: H264Profile
pattern H264Profile_MAIN = H264Profile' "MAIN"

{-# COMPLETE
  H264Profile_BASELINE,
  H264Profile_HIGH,
  H264Profile_HIGH_10BIT,
  H264Profile_HIGH_422,
  H264Profile_HIGH_422_10BIT,
  H264Profile_MAIN,
  H264Profile'
  #-}
