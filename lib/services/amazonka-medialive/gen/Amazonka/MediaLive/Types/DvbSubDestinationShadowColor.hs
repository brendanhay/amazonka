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
-- Module      : Amazonka.MediaLive.Types.DvbSubDestinationShadowColor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.DvbSubDestinationShadowColor
  ( DvbSubDestinationShadowColor
      ( ..,
        DvbSubDestinationShadowColor_BLACK,
        DvbSubDestinationShadowColor_NONE,
        DvbSubDestinationShadowColor_WHITE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Dvb Sub Destination Shadow Color
newtype DvbSubDestinationShadowColor = DvbSubDestinationShadowColor'
  { fromDvbSubDestinationShadowColor ::
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

pattern DvbSubDestinationShadowColor_BLACK :: DvbSubDestinationShadowColor
pattern DvbSubDestinationShadowColor_BLACK = DvbSubDestinationShadowColor' "BLACK"

pattern DvbSubDestinationShadowColor_NONE :: DvbSubDestinationShadowColor
pattern DvbSubDestinationShadowColor_NONE = DvbSubDestinationShadowColor' "NONE"

pattern DvbSubDestinationShadowColor_WHITE :: DvbSubDestinationShadowColor
pattern DvbSubDestinationShadowColor_WHITE = DvbSubDestinationShadowColor' "WHITE"

{-# COMPLETE
  DvbSubDestinationShadowColor_BLACK,
  DvbSubDestinationShadowColor_NONE,
  DvbSubDestinationShadowColor_WHITE,
  DvbSubDestinationShadowColor'
  #-}
