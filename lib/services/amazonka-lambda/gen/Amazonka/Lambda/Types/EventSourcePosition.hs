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
-- Module      : Amazonka.Lambda.Types.EventSourcePosition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.EventSourcePosition
  ( EventSourcePosition
      ( ..,
        EventSourcePosition_AT_TIMESTAMP,
        EventSourcePosition_LATEST,
        EventSourcePosition_TRIM_HORIZON
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EventSourcePosition = EventSourcePosition'
  { fromEventSourcePosition ::
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

pattern EventSourcePosition_AT_TIMESTAMP :: EventSourcePosition
pattern EventSourcePosition_AT_TIMESTAMP = EventSourcePosition' "AT_TIMESTAMP"

pattern EventSourcePosition_LATEST :: EventSourcePosition
pattern EventSourcePosition_LATEST = EventSourcePosition' "LATEST"

pattern EventSourcePosition_TRIM_HORIZON :: EventSourcePosition
pattern EventSourcePosition_TRIM_HORIZON = EventSourcePosition' "TRIM_HORIZON"

{-# COMPLETE
  EventSourcePosition_AT_TIMESTAMP,
  EventSourcePosition_LATEST,
  EventSourcePosition_TRIM_HORIZON,
  EventSourcePosition'
  #-}
