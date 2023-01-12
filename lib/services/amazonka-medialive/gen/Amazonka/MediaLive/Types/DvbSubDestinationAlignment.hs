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
-- Module      : Amazonka.MediaLive.Types.DvbSubDestinationAlignment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.DvbSubDestinationAlignment
  ( DvbSubDestinationAlignment
      ( ..,
        DvbSubDestinationAlignment_CENTERED,
        DvbSubDestinationAlignment_LEFT,
        DvbSubDestinationAlignment_SMART
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Dvb Sub Destination Alignment
newtype DvbSubDestinationAlignment = DvbSubDestinationAlignment'
  { fromDvbSubDestinationAlignment ::
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
