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
-- Module      : Network.AWS.MediaLive.Types.DvbSubDestinationShadowColor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.DvbSubDestinationShadowColor
  ( DvbSubDestinationShadowColor
      ( ..,
        DvbSubDestinationShadowColor_BLACK,
        DvbSubDestinationShadowColor_NONE,
        DvbSubDestinationShadowColor_WHITE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Dvb Sub Destination Shadow Color
newtype DvbSubDestinationShadowColor = DvbSubDestinationShadowColor'
  { fromDvbSubDestinationShadowColor ::
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
