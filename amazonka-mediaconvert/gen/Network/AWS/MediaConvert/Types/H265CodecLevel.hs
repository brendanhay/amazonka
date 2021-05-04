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
-- Module      : Network.AWS.MediaConvert.Types.H265CodecLevel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265CodecLevel
  ( H265CodecLevel
      ( ..,
        H265CodecLevel_AUTO,
        H265CodecLevel_LEVEL_1,
        H265CodecLevel_LEVEL_2,
        H265CodecLevel_LEVEL_2_1,
        H265CodecLevel_LEVEL_3,
        H265CodecLevel_LEVEL_3_1,
        H265CodecLevel_LEVEL_4,
        H265CodecLevel_LEVEL_4_1,
        H265CodecLevel_LEVEL_5,
        H265CodecLevel_LEVEL_5_1,
        H265CodecLevel_LEVEL_5_2,
        H265CodecLevel_LEVEL_6,
        H265CodecLevel_LEVEL_6_1,
        H265CodecLevel_LEVEL_6_2
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | H.265 Level.
newtype H265CodecLevel = H265CodecLevel'
  { fromH265CodecLevel ::
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

pattern H265CodecLevel_AUTO :: H265CodecLevel
pattern H265CodecLevel_AUTO = H265CodecLevel' "AUTO"

pattern H265CodecLevel_LEVEL_1 :: H265CodecLevel
pattern H265CodecLevel_LEVEL_1 = H265CodecLevel' "LEVEL_1"

pattern H265CodecLevel_LEVEL_2 :: H265CodecLevel
pattern H265CodecLevel_LEVEL_2 = H265CodecLevel' "LEVEL_2"

pattern H265CodecLevel_LEVEL_2_1 :: H265CodecLevel
pattern H265CodecLevel_LEVEL_2_1 = H265CodecLevel' "LEVEL_2_1"

pattern H265CodecLevel_LEVEL_3 :: H265CodecLevel
pattern H265CodecLevel_LEVEL_3 = H265CodecLevel' "LEVEL_3"

pattern H265CodecLevel_LEVEL_3_1 :: H265CodecLevel
pattern H265CodecLevel_LEVEL_3_1 = H265CodecLevel' "LEVEL_3_1"

pattern H265CodecLevel_LEVEL_4 :: H265CodecLevel
pattern H265CodecLevel_LEVEL_4 = H265CodecLevel' "LEVEL_4"

pattern H265CodecLevel_LEVEL_4_1 :: H265CodecLevel
pattern H265CodecLevel_LEVEL_4_1 = H265CodecLevel' "LEVEL_4_1"

pattern H265CodecLevel_LEVEL_5 :: H265CodecLevel
pattern H265CodecLevel_LEVEL_5 = H265CodecLevel' "LEVEL_5"

pattern H265CodecLevel_LEVEL_5_1 :: H265CodecLevel
pattern H265CodecLevel_LEVEL_5_1 = H265CodecLevel' "LEVEL_5_1"

pattern H265CodecLevel_LEVEL_5_2 :: H265CodecLevel
pattern H265CodecLevel_LEVEL_5_2 = H265CodecLevel' "LEVEL_5_2"

pattern H265CodecLevel_LEVEL_6 :: H265CodecLevel
pattern H265CodecLevel_LEVEL_6 = H265CodecLevel' "LEVEL_6"

pattern H265CodecLevel_LEVEL_6_1 :: H265CodecLevel
pattern H265CodecLevel_LEVEL_6_1 = H265CodecLevel' "LEVEL_6_1"

pattern H265CodecLevel_LEVEL_6_2 :: H265CodecLevel
pattern H265CodecLevel_LEVEL_6_2 = H265CodecLevel' "LEVEL_6_2"

{-# COMPLETE
  H265CodecLevel_AUTO,
  H265CodecLevel_LEVEL_1,
  H265CodecLevel_LEVEL_2,
  H265CodecLevel_LEVEL_2_1,
  H265CodecLevel_LEVEL_3,
  H265CodecLevel_LEVEL_3_1,
  H265CodecLevel_LEVEL_4,
  H265CodecLevel_LEVEL_4_1,
  H265CodecLevel_LEVEL_5,
  H265CodecLevel_LEVEL_5_1,
  H265CodecLevel_LEVEL_5_2,
  H265CodecLevel_LEVEL_6,
  H265CodecLevel_LEVEL_6_1,
  H265CodecLevel_LEVEL_6_2,
  H265CodecLevel'
  #-}
