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
-- Module      : Amazonka.Nimble.Types.StreamingSessionStreamState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StreamingSessionStreamState
  ( StreamingSessionStreamState
      ( ..,
        StreamingSessionStreamState_CREATE_FAILED,
        StreamingSessionStreamState_CREATE_IN_PROGRESS,
        StreamingSessionStreamState_DELETED,
        StreamingSessionStreamState_DELETE_FAILED,
        StreamingSessionStreamState_DELETE_IN_PROGRESS,
        StreamingSessionStreamState_READY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StreamingSessionStreamState = StreamingSessionStreamState'
  { fromStreamingSessionStreamState ::
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

pattern StreamingSessionStreamState_CREATE_FAILED :: StreamingSessionStreamState
pattern StreamingSessionStreamState_CREATE_FAILED = StreamingSessionStreamState' "CREATE_FAILED"

pattern StreamingSessionStreamState_CREATE_IN_PROGRESS :: StreamingSessionStreamState
pattern StreamingSessionStreamState_CREATE_IN_PROGRESS = StreamingSessionStreamState' "CREATE_IN_PROGRESS"

pattern StreamingSessionStreamState_DELETED :: StreamingSessionStreamState
pattern StreamingSessionStreamState_DELETED = StreamingSessionStreamState' "DELETED"

pattern StreamingSessionStreamState_DELETE_FAILED :: StreamingSessionStreamState
pattern StreamingSessionStreamState_DELETE_FAILED = StreamingSessionStreamState' "DELETE_FAILED"

pattern StreamingSessionStreamState_DELETE_IN_PROGRESS :: StreamingSessionStreamState
pattern StreamingSessionStreamState_DELETE_IN_PROGRESS = StreamingSessionStreamState' "DELETE_IN_PROGRESS"

pattern StreamingSessionStreamState_READY :: StreamingSessionStreamState
pattern StreamingSessionStreamState_READY = StreamingSessionStreamState' "READY"

{-# COMPLETE
  StreamingSessionStreamState_CREATE_FAILED,
  StreamingSessionStreamState_CREATE_IN_PROGRESS,
  StreamingSessionStreamState_DELETED,
  StreamingSessionStreamState_DELETE_FAILED,
  StreamingSessionStreamState_DELETE_IN_PROGRESS,
  StreamingSessionStreamState_READY,
  StreamingSessionStreamState'
  #-}
