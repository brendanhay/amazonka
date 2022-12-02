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
-- Module      : Amazonka.Nimble.Types.StudioComponentState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StudioComponentState
  ( StudioComponentState
      ( ..,
        StudioComponentState_CREATE_FAILED,
        StudioComponentState_CREATE_IN_PROGRESS,
        StudioComponentState_DELETED,
        StudioComponentState_DELETE_FAILED,
        StudioComponentState_DELETE_IN_PROGRESS,
        StudioComponentState_READY,
        StudioComponentState_UPDATE_FAILED,
        StudioComponentState_UPDATE_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The current state of the studio component resource.
--
-- While a studio component is being created, modified, or deleted, its
-- state will equal \"CREATE_IN_PROGRESS\", \"UPDATE_IN_PROGRESS\", or
-- \"DELETE_IN_PROGRESS\"
--
-- These are called \'transition states\'.
--
-- No modifications may be made to the studio component while it is in a
-- transition state.
--
-- If creation of the resource fails, the state will change to
-- \`CREATE_FAILED\`. The resource StatusCode and StatusMessage will
-- provide more information of why creation failed. The resource in this
-- state will automatically be deleted from your account after a period of
-- time.
--
-- If updating the resource fails, the state will change to
-- \`UPDATE_FAILED\`. The resource StatusCode and StatusMessage will
-- provide more information of why the update failed. The resource will be
-- returned to the state it was in when the update request was invoked.
--
-- If deleting the resource fails, the state will change to
-- \`DELETE_FAILED\`. The resource StatusCode and StatusMessage will
-- provide more information of why the update failed. The resource will be
-- returned to the state it was in when the update request was invoked.
-- After the resource is deleted successfully, it will change to the
-- \"DELETED\" state. The resource will no longer count against service
-- quotas and cannot be used or acted upon any futher. It will be removed
-- from your account after a period of time.
newtype StudioComponentState = StudioComponentState'
  { fromStudioComponentState ::
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

pattern StudioComponentState_CREATE_FAILED :: StudioComponentState
pattern StudioComponentState_CREATE_FAILED = StudioComponentState' "CREATE_FAILED"

pattern StudioComponentState_CREATE_IN_PROGRESS :: StudioComponentState
pattern StudioComponentState_CREATE_IN_PROGRESS = StudioComponentState' "CREATE_IN_PROGRESS"

pattern StudioComponentState_DELETED :: StudioComponentState
pattern StudioComponentState_DELETED = StudioComponentState' "DELETED"

pattern StudioComponentState_DELETE_FAILED :: StudioComponentState
pattern StudioComponentState_DELETE_FAILED = StudioComponentState' "DELETE_FAILED"

pattern StudioComponentState_DELETE_IN_PROGRESS :: StudioComponentState
pattern StudioComponentState_DELETE_IN_PROGRESS = StudioComponentState' "DELETE_IN_PROGRESS"

pattern StudioComponentState_READY :: StudioComponentState
pattern StudioComponentState_READY = StudioComponentState' "READY"

pattern StudioComponentState_UPDATE_FAILED :: StudioComponentState
pattern StudioComponentState_UPDATE_FAILED = StudioComponentState' "UPDATE_FAILED"

pattern StudioComponentState_UPDATE_IN_PROGRESS :: StudioComponentState
pattern StudioComponentState_UPDATE_IN_PROGRESS = StudioComponentState' "UPDATE_IN_PROGRESS"

{-# COMPLETE
  StudioComponentState_CREATE_FAILED,
  StudioComponentState_CREATE_IN_PROGRESS,
  StudioComponentState_DELETED,
  StudioComponentState_DELETE_FAILED,
  StudioComponentState_DELETE_IN_PROGRESS,
  StudioComponentState_READY,
  StudioComponentState_UPDATE_FAILED,
  StudioComponentState_UPDATE_IN_PROGRESS,
  StudioComponentState'
  #-}
