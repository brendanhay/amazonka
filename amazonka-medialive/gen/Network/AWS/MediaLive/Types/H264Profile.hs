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

import qualified Network.AWS.Prelude as Prelude

-- | H264 Profile
newtype H264Profile = H264Profile'
  { fromH264Profile ::
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
