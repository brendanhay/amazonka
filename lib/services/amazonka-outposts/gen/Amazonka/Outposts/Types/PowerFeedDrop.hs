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
-- Module      : Amazonka.Outposts.Types.PowerFeedDrop
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types.PowerFeedDrop
  ( PowerFeedDrop
      ( ..,
        PowerFeedDrop_ABOVE_RACK,
        PowerFeedDrop_BELOW_RACK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype PowerFeedDrop = PowerFeedDrop'
  { fromPowerFeedDrop ::
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

pattern PowerFeedDrop_ABOVE_RACK :: PowerFeedDrop
pattern PowerFeedDrop_ABOVE_RACK = PowerFeedDrop' "ABOVE_RACK"

pattern PowerFeedDrop_BELOW_RACK :: PowerFeedDrop
pattern PowerFeedDrop_BELOW_RACK = PowerFeedDrop' "BELOW_RACK"

{-# COMPLETE
  PowerFeedDrop_ABOVE_RACK,
  PowerFeedDrop_BELOW_RACK,
  PowerFeedDrop'
  #-}
