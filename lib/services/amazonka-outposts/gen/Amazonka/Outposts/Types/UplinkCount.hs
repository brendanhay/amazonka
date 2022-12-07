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
-- Module      : Amazonka.Outposts.Types.UplinkCount
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types.UplinkCount
  ( UplinkCount
      ( ..,
        UplinkCount_UPLINK_COUNT_1,
        UplinkCount_UPLINK_COUNT_12,
        UplinkCount_UPLINK_COUNT_16,
        UplinkCount_UPLINK_COUNT_2,
        UplinkCount_UPLINK_COUNT_3,
        UplinkCount_UPLINK_COUNT_4,
        UplinkCount_UPLINK_COUNT_5,
        UplinkCount_UPLINK_COUNT_6,
        UplinkCount_UPLINK_COUNT_7,
        UplinkCount_UPLINK_COUNT_8
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype UplinkCount = UplinkCount'
  { fromUplinkCount ::
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

pattern UplinkCount_UPLINK_COUNT_1 :: UplinkCount
pattern UplinkCount_UPLINK_COUNT_1 = UplinkCount' "UPLINK_COUNT_1"

pattern UplinkCount_UPLINK_COUNT_12 :: UplinkCount
pattern UplinkCount_UPLINK_COUNT_12 = UplinkCount' "UPLINK_COUNT_12"

pattern UplinkCount_UPLINK_COUNT_16 :: UplinkCount
pattern UplinkCount_UPLINK_COUNT_16 = UplinkCount' "UPLINK_COUNT_16"

pattern UplinkCount_UPLINK_COUNT_2 :: UplinkCount
pattern UplinkCount_UPLINK_COUNT_2 = UplinkCount' "UPLINK_COUNT_2"

pattern UplinkCount_UPLINK_COUNT_3 :: UplinkCount
pattern UplinkCount_UPLINK_COUNT_3 = UplinkCount' "UPLINK_COUNT_3"

pattern UplinkCount_UPLINK_COUNT_4 :: UplinkCount
pattern UplinkCount_UPLINK_COUNT_4 = UplinkCount' "UPLINK_COUNT_4"

pattern UplinkCount_UPLINK_COUNT_5 :: UplinkCount
pattern UplinkCount_UPLINK_COUNT_5 = UplinkCount' "UPLINK_COUNT_5"

pattern UplinkCount_UPLINK_COUNT_6 :: UplinkCount
pattern UplinkCount_UPLINK_COUNT_6 = UplinkCount' "UPLINK_COUNT_6"

pattern UplinkCount_UPLINK_COUNT_7 :: UplinkCount
pattern UplinkCount_UPLINK_COUNT_7 = UplinkCount' "UPLINK_COUNT_7"

pattern UplinkCount_UPLINK_COUNT_8 :: UplinkCount
pattern UplinkCount_UPLINK_COUNT_8 = UplinkCount' "UPLINK_COUNT_8"

{-# COMPLETE
  UplinkCount_UPLINK_COUNT_1,
  UplinkCount_UPLINK_COUNT_12,
  UplinkCount_UPLINK_COUNT_16,
  UplinkCount_UPLINK_COUNT_2,
  UplinkCount_UPLINK_COUNT_3,
  UplinkCount_UPLINK_COUNT_4,
  UplinkCount_UPLINK_COUNT_5,
  UplinkCount_UPLINK_COUNT_6,
  UplinkCount_UPLINK_COUNT_7,
  UplinkCount_UPLINK_COUNT_8,
  UplinkCount'
  #-}
