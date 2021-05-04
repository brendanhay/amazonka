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
-- Module      : Network.AWS.MediaLive.Types.DvbSubDestinationAlignment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.DvbSubDestinationAlignment
  ( DvbSubDestinationAlignment
      ( ..,
        DvbSubDestinationAlignment_CENTERED,
        DvbSubDestinationAlignment_LEFT,
        DvbSubDestinationAlignment_SMART
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Dvb Sub Destination Alignment
newtype DvbSubDestinationAlignment = DvbSubDestinationAlignment'
  { fromDvbSubDestinationAlignment ::
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

pattern DvbSubDestinationAlignment_CENTERED :: DvbSubDestinationAlignment
pattern DvbSubDestinationAlignment_CENTERED = DvbSubDestinationAlignment' "CENTERED"

pattern DvbSubDestinationAlignment_LEFT :: DvbSubDestinationAlignment
pattern DvbSubDestinationAlignment_LEFT = DvbSubDestinationAlignment' "LEFT"

pattern DvbSubDestinationAlignment_SMART :: DvbSubDestinationAlignment
pattern DvbSubDestinationAlignment_SMART = DvbSubDestinationAlignment' "SMART"

{-# COMPLETE
  DvbSubDestinationAlignment_CENTERED,
  DvbSubDestinationAlignment_LEFT,
  DvbSubDestinationAlignment_SMART,
  DvbSubDestinationAlignment'
  #-}
