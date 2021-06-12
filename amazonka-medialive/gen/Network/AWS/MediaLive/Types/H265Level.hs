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
-- Module      : Network.AWS.MediaLive.Types.H265Level
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265Level
  ( H265Level
      ( ..,
        H265Level_H265_LEVEL_1,
        H265Level_H265_LEVEL_2,
        H265Level_H265_LEVEL_2_1,
        H265Level_H265_LEVEL_3,
        H265Level_H265_LEVEL_3_1,
        H265Level_H265_LEVEL_4,
        H265Level_H265_LEVEL_4_1,
        H265Level_H265_LEVEL_5,
        H265Level_H265_LEVEL_5_1,
        H265Level_H265_LEVEL_5_2,
        H265Level_H265_LEVEL_6,
        H265Level_H265_LEVEL_6_1,
        H265Level_H265_LEVEL_6_2,
        H265Level_H265_LEVEL_AUTO
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | H265 Level
newtype H265Level = H265Level'
  { fromH265Level ::
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

pattern H265Level_H265_LEVEL_1 :: H265Level
pattern H265Level_H265_LEVEL_1 = H265Level' "H265_LEVEL_1"

pattern H265Level_H265_LEVEL_2 :: H265Level
pattern H265Level_H265_LEVEL_2 = H265Level' "H265_LEVEL_2"

pattern H265Level_H265_LEVEL_2_1 :: H265Level
pattern H265Level_H265_LEVEL_2_1 = H265Level' "H265_LEVEL_2_1"

pattern H265Level_H265_LEVEL_3 :: H265Level
pattern H265Level_H265_LEVEL_3 = H265Level' "H265_LEVEL_3"

pattern H265Level_H265_LEVEL_3_1 :: H265Level
pattern H265Level_H265_LEVEL_3_1 = H265Level' "H265_LEVEL_3_1"

pattern H265Level_H265_LEVEL_4 :: H265Level
pattern H265Level_H265_LEVEL_4 = H265Level' "H265_LEVEL_4"

pattern H265Level_H265_LEVEL_4_1 :: H265Level
pattern H265Level_H265_LEVEL_4_1 = H265Level' "H265_LEVEL_4_1"

pattern H265Level_H265_LEVEL_5 :: H265Level
pattern H265Level_H265_LEVEL_5 = H265Level' "H265_LEVEL_5"

pattern H265Level_H265_LEVEL_5_1 :: H265Level
pattern H265Level_H265_LEVEL_5_1 = H265Level' "H265_LEVEL_5_1"

pattern H265Level_H265_LEVEL_5_2 :: H265Level
pattern H265Level_H265_LEVEL_5_2 = H265Level' "H265_LEVEL_5_2"

pattern H265Level_H265_LEVEL_6 :: H265Level
pattern H265Level_H265_LEVEL_6 = H265Level' "H265_LEVEL_6"

pattern H265Level_H265_LEVEL_6_1 :: H265Level
pattern H265Level_H265_LEVEL_6_1 = H265Level' "H265_LEVEL_6_1"

pattern H265Level_H265_LEVEL_6_2 :: H265Level
pattern H265Level_H265_LEVEL_6_2 = H265Level' "H265_LEVEL_6_2"

pattern H265Level_H265_LEVEL_AUTO :: H265Level
pattern H265Level_H265_LEVEL_AUTO = H265Level' "H265_LEVEL_AUTO"

{-# COMPLETE
  H265Level_H265_LEVEL_1,
  H265Level_H265_LEVEL_2,
  H265Level_H265_LEVEL_2_1,
  H265Level_H265_LEVEL_3,
  H265Level_H265_LEVEL_3_1,
  H265Level_H265_LEVEL_4,
  H265Level_H265_LEVEL_4_1,
  H265Level_H265_LEVEL_5,
  H265Level_H265_LEVEL_5_1,
  H265Level_H265_LEVEL_5_2,
  H265Level_H265_LEVEL_6,
  H265Level_H265_LEVEL_6_1,
  H265Level_H265_LEVEL_6_2,
  H265Level_H265_LEVEL_AUTO,
  H265Level'
  #-}
