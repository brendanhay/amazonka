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
-- Module      : Network.AWS.MediaConvert.Types.H264CodecLevel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264CodecLevel
  ( H264CodecLevel
      ( ..,
        H264CodecLevel_AUTO,
        H264CodecLevel_LEVEL_1,
        H264CodecLevel_LEVEL_1_1,
        H264CodecLevel_LEVEL_1_2,
        H264CodecLevel_LEVEL_1_3,
        H264CodecLevel_LEVEL_2,
        H264CodecLevel_LEVEL_2_1,
        H264CodecLevel_LEVEL_2_2,
        H264CodecLevel_LEVEL_3,
        H264CodecLevel_LEVEL_3_1,
        H264CodecLevel_LEVEL_3_2,
        H264CodecLevel_LEVEL_4,
        H264CodecLevel_LEVEL_4_1,
        H264CodecLevel_LEVEL_4_2,
        H264CodecLevel_LEVEL_5,
        H264CodecLevel_LEVEL_5_1,
        H264CodecLevel_LEVEL_5_2
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Specify an H.264 level that is consistent with your output video
-- settings. If you aren\'t sure what level to specify, choose Auto (AUTO).
newtype H264CodecLevel = H264CodecLevel'
  { fromH264CodecLevel ::
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

pattern H264CodecLevel_AUTO :: H264CodecLevel
pattern H264CodecLevel_AUTO = H264CodecLevel' "AUTO"

pattern H264CodecLevel_LEVEL_1 :: H264CodecLevel
pattern H264CodecLevel_LEVEL_1 = H264CodecLevel' "LEVEL_1"

pattern H264CodecLevel_LEVEL_1_1 :: H264CodecLevel
pattern H264CodecLevel_LEVEL_1_1 = H264CodecLevel' "LEVEL_1_1"

pattern H264CodecLevel_LEVEL_1_2 :: H264CodecLevel
pattern H264CodecLevel_LEVEL_1_2 = H264CodecLevel' "LEVEL_1_2"

pattern H264CodecLevel_LEVEL_1_3 :: H264CodecLevel
pattern H264CodecLevel_LEVEL_1_3 = H264CodecLevel' "LEVEL_1_3"

pattern H264CodecLevel_LEVEL_2 :: H264CodecLevel
pattern H264CodecLevel_LEVEL_2 = H264CodecLevel' "LEVEL_2"

pattern H264CodecLevel_LEVEL_2_1 :: H264CodecLevel
pattern H264CodecLevel_LEVEL_2_1 = H264CodecLevel' "LEVEL_2_1"

pattern H264CodecLevel_LEVEL_2_2 :: H264CodecLevel
pattern H264CodecLevel_LEVEL_2_2 = H264CodecLevel' "LEVEL_2_2"

pattern H264CodecLevel_LEVEL_3 :: H264CodecLevel
pattern H264CodecLevel_LEVEL_3 = H264CodecLevel' "LEVEL_3"

pattern H264CodecLevel_LEVEL_3_1 :: H264CodecLevel
pattern H264CodecLevel_LEVEL_3_1 = H264CodecLevel' "LEVEL_3_1"

pattern H264CodecLevel_LEVEL_3_2 :: H264CodecLevel
pattern H264CodecLevel_LEVEL_3_2 = H264CodecLevel' "LEVEL_3_2"

pattern H264CodecLevel_LEVEL_4 :: H264CodecLevel
pattern H264CodecLevel_LEVEL_4 = H264CodecLevel' "LEVEL_4"

pattern H264CodecLevel_LEVEL_4_1 :: H264CodecLevel
pattern H264CodecLevel_LEVEL_4_1 = H264CodecLevel' "LEVEL_4_1"

pattern H264CodecLevel_LEVEL_4_2 :: H264CodecLevel
pattern H264CodecLevel_LEVEL_4_2 = H264CodecLevel' "LEVEL_4_2"

pattern H264CodecLevel_LEVEL_5 :: H264CodecLevel
pattern H264CodecLevel_LEVEL_5 = H264CodecLevel' "LEVEL_5"

pattern H264CodecLevel_LEVEL_5_1 :: H264CodecLevel
pattern H264CodecLevel_LEVEL_5_1 = H264CodecLevel' "LEVEL_5_1"

pattern H264CodecLevel_LEVEL_5_2 :: H264CodecLevel
pattern H264CodecLevel_LEVEL_5_2 = H264CodecLevel' "LEVEL_5_2"

{-# COMPLETE
  H264CodecLevel_AUTO,
  H264CodecLevel_LEVEL_1,
  H264CodecLevel_LEVEL_1_1,
  H264CodecLevel_LEVEL_1_2,
  H264CodecLevel_LEVEL_1_3,
  H264CodecLevel_LEVEL_2,
  H264CodecLevel_LEVEL_2_1,
  H264CodecLevel_LEVEL_2_2,
  H264CodecLevel_LEVEL_3,
  H264CodecLevel_LEVEL_3_1,
  H264CodecLevel_LEVEL_3_2,
  H264CodecLevel_LEVEL_4,
  H264CodecLevel_LEVEL_4_1,
  H264CodecLevel_LEVEL_4_2,
  H264CodecLevel_LEVEL_5,
  H264CodecLevel_LEVEL_5_1,
  H264CodecLevel_LEVEL_5_2,
  H264CodecLevel'
  #-}
