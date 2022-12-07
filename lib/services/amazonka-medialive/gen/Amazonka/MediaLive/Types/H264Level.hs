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
-- Module      : Amazonka.MediaLive.Types.H264Level
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.H264Level
  ( H264Level
      ( ..,
        H264Level_H264_LEVEL_1,
        H264Level_H264_LEVEL_1_1,
        H264Level_H264_LEVEL_1_2,
        H264Level_H264_LEVEL_1_3,
        H264Level_H264_LEVEL_2,
        H264Level_H264_LEVEL_2_1,
        H264Level_H264_LEVEL_2_2,
        H264Level_H264_LEVEL_3,
        H264Level_H264_LEVEL_3_1,
        H264Level_H264_LEVEL_3_2,
        H264Level_H264_LEVEL_4,
        H264Level_H264_LEVEL_4_1,
        H264Level_H264_LEVEL_4_2,
        H264Level_H264_LEVEL_5,
        H264Level_H264_LEVEL_5_1,
        H264Level_H264_LEVEL_5_2,
        H264Level_H264_LEVEL_AUTO
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | H264 Level
newtype H264Level = H264Level'
  { fromH264Level ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern H264Level_H264_LEVEL_1 :: H264Level
pattern H264Level_H264_LEVEL_1 = H264Level' "H264_LEVEL_1"

pattern H264Level_H264_LEVEL_1_1 :: H264Level
pattern H264Level_H264_LEVEL_1_1 = H264Level' "H264_LEVEL_1_1"

pattern H264Level_H264_LEVEL_1_2 :: H264Level
pattern H264Level_H264_LEVEL_1_2 = H264Level' "H264_LEVEL_1_2"

pattern H264Level_H264_LEVEL_1_3 :: H264Level
pattern H264Level_H264_LEVEL_1_3 = H264Level' "H264_LEVEL_1_3"

pattern H264Level_H264_LEVEL_2 :: H264Level
pattern H264Level_H264_LEVEL_2 = H264Level' "H264_LEVEL_2"

pattern H264Level_H264_LEVEL_2_1 :: H264Level
pattern H264Level_H264_LEVEL_2_1 = H264Level' "H264_LEVEL_2_1"

pattern H264Level_H264_LEVEL_2_2 :: H264Level
pattern H264Level_H264_LEVEL_2_2 = H264Level' "H264_LEVEL_2_2"

pattern H264Level_H264_LEVEL_3 :: H264Level
pattern H264Level_H264_LEVEL_3 = H264Level' "H264_LEVEL_3"

pattern H264Level_H264_LEVEL_3_1 :: H264Level
pattern H264Level_H264_LEVEL_3_1 = H264Level' "H264_LEVEL_3_1"

pattern H264Level_H264_LEVEL_3_2 :: H264Level
pattern H264Level_H264_LEVEL_3_2 = H264Level' "H264_LEVEL_3_2"

pattern H264Level_H264_LEVEL_4 :: H264Level
pattern H264Level_H264_LEVEL_4 = H264Level' "H264_LEVEL_4"

pattern H264Level_H264_LEVEL_4_1 :: H264Level
pattern H264Level_H264_LEVEL_4_1 = H264Level' "H264_LEVEL_4_1"

pattern H264Level_H264_LEVEL_4_2 :: H264Level
pattern H264Level_H264_LEVEL_4_2 = H264Level' "H264_LEVEL_4_2"

pattern H264Level_H264_LEVEL_5 :: H264Level
pattern H264Level_H264_LEVEL_5 = H264Level' "H264_LEVEL_5"

pattern H264Level_H264_LEVEL_5_1 :: H264Level
pattern H264Level_H264_LEVEL_5_1 = H264Level' "H264_LEVEL_5_1"

pattern H264Level_H264_LEVEL_5_2 :: H264Level
pattern H264Level_H264_LEVEL_5_2 = H264Level' "H264_LEVEL_5_2"

pattern H264Level_H264_LEVEL_AUTO :: H264Level
pattern H264Level_H264_LEVEL_AUTO = H264Level' "H264_LEVEL_AUTO"

{-# COMPLETE
  H264Level_H264_LEVEL_1,
  H264Level_H264_LEVEL_1_1,
  H264Level_H264_LEVEL_1_2,
  H264Level_H264_LEVEL_1_3,
  H264Level_H264_LEVEL_2,
  H264Level_H264_LEVEL_2_1,
  H264Level_H264_LEVEL_2_2,
  H264Level_H264_LEVEL_3,
  H264Level_H264_LEVEL_3_1,
  H264Level_H264_LEVEL_3_2,
  H264Level_H264_LEVEL_4,
  H264Level_H264_LEVEL_4_1,
  H264Level_H264_LEVEL_4_2,
  H264Level_H264_LEVEL_5,
  H264Level_H264_LEVEL_5_1,
  H264Level_H264_LEVEL_5_2,
  H264Level_H264_LEVEL_AUTO,
  H264Level'
  #-}
