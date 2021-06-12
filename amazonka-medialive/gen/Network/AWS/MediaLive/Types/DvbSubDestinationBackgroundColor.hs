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
-- Module      : Network.AWS.MediaLive.Types.DvbSubDestinationBackgroundColor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.DvbSubDestinationBackgroundColor
  ( DvbSubDestinationBackgroundColor
      ( ..,
        DvbSubDestinationBackgroundColor_BLACK,
        DvbSubDestinationBackgroundColor_NONE,
        DvbSubDestinationBackgroundColor_WHITE
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Dvb Sub Destination Background Color
newtype DvbSubDestinationBackgroundColor = DvbSubDestinationBackgroundColor'
  { fromDvbSubDestinationBackgroundColor ::
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

pattern DvbSubDestinationBackgroundColor_BLACK :: DvbSubDestinationBackgroundColor
pattern DvbSubDestinationBackgroundColor_BLACK = DvbSubDestinationBackgroundColor' "BLACK"

pattern DvbSubDestinationBackgroundColor_NONE :: DvbSubDestinationBackgroundColor
pattern DvbSubDestinationBackgroundColor_NONE = DvbSubDestinationBackgroundColor' "NONE"

pattern DvbSubDestinationBackgroundColor_WHITE :: DvbSubDestinationBackgroundColor
pattern DvbSubDestinationBackgroundColor_WHITE = DvbSubDestinationBackgroundColor' "WHITE"

{-# COMPLETE
  DvbSubDestinationBackgroundColor_BLACK,
  DvbSubDestinationBackgroundColor_NONE,
  DvbSubDestinationBackgroundColor_WHITE,
  DvbSubDestinationBackgroundColor'
  #-}
