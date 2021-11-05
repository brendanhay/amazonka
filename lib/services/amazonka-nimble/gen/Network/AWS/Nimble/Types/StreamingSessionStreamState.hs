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
-- Module      : Network.AWS.Nimble.Types.StreamingSessionStreamState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Nimble.Types.StreamingSessionStreamState
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- |
newtype StreamingSessionStreamState = StreamingSessionStreamState'
  { fromStreamingSessionStreamState ::
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
