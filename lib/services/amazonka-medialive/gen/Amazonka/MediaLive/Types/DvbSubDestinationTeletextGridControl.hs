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
-- Module      : Amazonka.MediaLive.Types.DvbSubDestinationTeletextGridControl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.DvbSubDestinationTeletextGridControl
  ( DvbSubDestinationTeletextGridControl
      ( ..,
        DvbSubDestinationTeletextGridControl_FIXED,
        DvbSubDestinationTeletextGridControl_SCALED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Dvb Sub Destination Teletext Grid Control
newtype DvbSubDestinationTeletextGridControl = DvbSubDestinationTeletextGridControl'
  { fromDvbSubDestinationTeletextGridControl ::
      Core.Text
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

pattern DvbSubDestinationTeletextGridControl_FIXED :: DvbSubDestinationTeletextGridControl
pattern DvbSubDestinationTeletextGridControl_FIXED = DvbSubDestinationTeletextGridControl' "FIXED"

pattern DvbSubDestinationTeletextGridControl_SCALED :: DvbSubDestinationTeletextGridControl
pattern DvbSubDestinationTeletextGridControl_SCALED = DvbSubDestinationTeletextGridControl' "SCALED"

{-# COMPLETE
  DvbSubDestinationTeletextGridControl_FIXED,
  DvbSubDestinationTeletextGridControl_SCALED,
  DvbSubDestinationTeletextGridControl'
  #-}
