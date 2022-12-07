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
-- Module      : Amazonka.MediaLive.Types.H265Level
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.H265Level
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | H265 Level
newtype H265Level = H265Level'
  { fromH265Level ::
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
