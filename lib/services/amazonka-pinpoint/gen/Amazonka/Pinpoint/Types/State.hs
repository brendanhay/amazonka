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
-- Module      : Amazonka.Pinpoint.Types.State
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.State
  ( State
      ( ..,
        State_ACTIVE,
        State_CANCELLED,
        State_CLOSED,
        State_COMPLETED,
        State_DRAFT,
        State_PAUSED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype State = State' {fromState :: Data.Text}
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

pattern State_ACTIVE :: State
pattern State_ACTIVE = State' "ACTIVE"

pattern State_CANCELLED :: State
pattern State_CANCELLED = State' "CANCELLED"

pattern State_CLOSED :: State
pattern State_CLOSED = State' "CLOSED"

pattern State_COMPLETED :: State
pattern State_COMPLETED = State' "COMPLETED"

pattern State_DRAFT :: State
pattern State_DRAFT = State' "DRAFT"

pattern State_PAUSED :: State
pattern State_PAUSED = State' "PAUSED"

{-# COMPLETE
  State_ACTIVE,
  State_CANCELLED,
  State_CLOSED,
  State_COMPLETED,
  State_DRAFT,
  State_PAUSED,
  State'
  #-}
