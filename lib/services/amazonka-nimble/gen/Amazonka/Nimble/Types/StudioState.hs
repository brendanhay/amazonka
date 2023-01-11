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
-- Module      : Amazonka.Nimble.Types.StudioState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StudioState
  ( StudioState
      ( ..,
        StudioState_CREATE_FAILED,
        StudioState_CREATE_IN_PROGRESS,
        StudioState_DELETED,
        StudioState_DELETE_FAILED,
        StudioState_DELETE_IN_PROGRESS,
        StudioState_READY,
        StudioState_UPDATE_FAILED,
        StudioState_UPDATE_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StudioState = StudioState'
  { fromStudioState ::
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

pattern StudioState_CREATE_FAILED :: StudioState
pattern StudioState_CREATE_FAILED = StudioState' "CREATE_FAILED"

pattern StudioState_CREATE_IN_PROGRESS :: StudioState
pattern StudioState_CREATE_IN_PROGRESS = StudioState' "CREATE_IN_PROGRESS"

pattern StudioState_DELETED :: StudioState
pattern StudioState_DELETED = StudioState' "DELETED"

pattern StudioState_DELETE_FAILED :: StudioState
pattern StudioState_DELETE_FAILED = StudioState' "DELETE_FAILED"

pattern StudioState_DELETE_IN_PROGRESS :: StudioState
pattern StudioState_DELETE_IN_PROGRESS = StudioState' "DELETE_IN_PROGRESS"

pattern StudioState_READY :: StudioState
pattern StudioState_READY = StudioState' "READY"

pattern StudioState_UPDATE_FAILED :: StudioState
pattern StudioState_UPDATE_FAILED = StudioState' "UPDATE_FAILED"

pattern StudioState_UPDATE_IN_PROGRESS :: StudioState
pattern StudioState_UPDATE_IN_PROGRESS = StudioState' "UPDATE_IN_PROGRESS"

{-# COMPLETE
  StudioState_CREATE_FAILED,
  StudioState_CREATE_IN_PROGRESS,
  StudioState_DELETED,
  StudioState_DELETE_FAILED,
  StudioState_DELETE_IN_PROGRESS,
  StudioState_READY,
  StudioState_UPDATE_FAILED,
  StudioState_UPDATE_IN_PROGRESS,
  StudioState'
  #-}
