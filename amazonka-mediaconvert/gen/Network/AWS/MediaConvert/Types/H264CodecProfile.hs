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
-- Module      : Network.AWS.MediaConvert.Types.H264CodecProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264CodecProfile
  ( H264CodecProfile
      ( ..,
        H264CodecProfile_BASELINE,
        H264CodecProfile_HIGH,
        H264CodecProfile_HIGH_10BIT,
        H264CodecProfile_HIGH_422,
        H264CodecProfile_HIGH_422_10BIT,
        H264CodecProfile_MAIN
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | H.264 Profile. High 4:2:2 and 10-bit profiles are only available with
-- the AVC-I License.
newtype H264CodecProfile = H264CodecProfile'
  { fromH264CodecProfile ::
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

pattern H264CodecProfile_BASELINE :: H264CodecProfile
pattern H264CodecProfile_BASELINE = H264CodecProfile' "BASELINE"

pattern H264CodecProfile_HIGH :: H264CodecProfile
pattern H264CodecProfile_HIGH = H264CodecProfile' "HIGH"

pattern H264CodecProfile_HIGH_10BIT :: H264CodecProfile
pattern H264CodecProfile_HIGH_10BIT = H264CodecProfile' "HIGH_10BIT"

pattern H264CodecProfile_HIGH_422 :: H264CodecProfile
pattern H264CodecProfile_HIGH_422 = H264CodecProfile' "HIGH_422"

pattern H264CodecProfile_HIGH_422_10BIT :: H264CodecProfile
pattern H264CodecProfile_HIGH_422_10BIT = H264CodecProfile' "HIGH_422_10BIT"

pattern H264CodecProfile_MAIN :: H264CodecProfile
pattern H264CodecProfile_MAIN = H264CodecProfile' "MAIN"

{-# COMPLETE
  H264CodecProfile_BASELINE,
  H264CodecProfile_HIGH,
  H264CodecProfile_HIGH_10BIT,
  H264CodecProfile_HIGH_422,
  H264CodecProfile_HIGH_422_10BIT,
  H264CodecProfile_MAIN,
  H264CodecProfile'
  #-}
