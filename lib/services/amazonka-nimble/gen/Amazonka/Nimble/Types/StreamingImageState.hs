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
-- Module      : Amazonka.Nimble.Types.StreamingImageState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StreamingImageState
  ( StreamingImageState
      ( ..,
        StreamingImageState_CREATE_FAILED,
        StreamingImageState_CREATE_IN_PROGRESS,
        StreamingImageState_DELETED,
        StreamingImageState_DELETE_FAILED,
        StreamingImageState_DELETE_IN_PROGRESS,
        StreamingImageState_READY,
        StreamingImageState_UPDATE_FAILED,
        StreamingImageState_UPDATE_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype StreamingImageState = StreamingImageState'
  { fromStreamingImageState ::
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

pattern StreamingImageState_CREATE_FAILED :: StreamingImageState
pattern StreamingImageState_CREATE_FAILED = StreamingImageState' "CREATE_FAILED"

pattern StreamingImageState_CREATE_IN_PROGRESS :: StreamingImageState
pattern StreamingImageState_CREATE_IN_PROGRESS = StreamingImageState' "CREATE_IN_PROGRESS"

pattern StreamingImageState_DELETED :: StreamingImageState
pattern StreamingImageState_DELETED = StreamingImageState' "DELETED"

pattern StreamingImageState_DELETE_FAILED :: StreamingImageState
pattern StreamingImageState_DELETE_FAILED = StreamingImageState' "DELETE_FAILED"

pattern StreamingImageState_DELETE_IN_PROGRESS :: StreamingImageState
pattern StreamingImageState_DELETE_IN_PROGRESS = StreamingImageState' "DELETE_IN_PROGRESS"

pattern StreamingImageState_READY :: StreamingImageState
pattern StreamingImageState_READY = StreamingImageState' "READY"

pattern StreamingImageState_UPDATE_FAILED :: StreamingImageState
pattern StreamingImageState_UPDATE_FAILED = StreamingImageState' "UPDATE_FAILED"

pattern StreamingImageState_UPDATE_IN_PROGRESS :: StreamingImageState
pattern StreamingImageState_UPDATE_IN_PROGRESS = StreamingImageState' "UPDATE_IN_PROGRESS"

{-# COMPLETE
  StreamingImageState_CREATE_FAILED,
  StreamingImageState_CREATE_IN_PROGRESS,
  StreamingImageState_DELETED,
  StreamingImageState_DELETE_FAILED,
  StreamingImageState_DELETE_IN_PROGRESS,
  StreamingImageState_READY,
  StreamingImageState_UPDATE_FAILED,
  StreamingImageState_UPDATE_IN_PROGRESS,
  StreamingImageState'
  #-}
