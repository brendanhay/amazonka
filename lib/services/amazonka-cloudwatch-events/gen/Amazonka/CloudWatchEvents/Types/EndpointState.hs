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
-- Module      : Amazonka.CloudWatchEvents.Types.EndpointState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.EndpointState
  ( EndpointState
      ( ..,
        EndpointState_ACTIVE,
        EndpointState_CREATE_FAILED,
        EndpointState_CREATING,
        EndpointState_DELETE_FAILED,
        EndpointState_DELETING,
        EndpointState_UPDATE_FAILED,
        EndpointState_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EndpointState = EndpointState'
  { fromEndpointState ::
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

pattern EndpointState_ACTIVE :: EndpointState
pattern EndpointState_ACTIVE = EndpointState' "ACTIVE"

pattern EndpointState_CREATE_FAILED :: EndpointState
pattern EndpointState_CREATE_FAILED = EndpointState' "CREATE_FAILED"

pattern EndpointState_CREATING :: EndpointState
pattern EndpointState_CREATING = EndpointState' "CREATING"

pattern EndpointState_DELETE_FAILED :: EndpointState
pattern EndpointState_DELETE_FAILED = EndpointState' "DELETE_FAILED"

pattern EndpointState_DELETING :: EndpointState
pattern EndpointState_DELETING = EndpointState' "DELETING"

pattern EndpointState_UPDATE_FAILED :: EndpointState
pattern EndpointState_UPDATE_FAILED = EndpointState' "UPDATE_FAILED"

pattern EndpointState_UPDATING :: EndpointState
pattern EndpointState_UPDATING = EndpointState' "UPDATING"

{-# COMPLETE
  EndpointState_ACTIVE,
  EndpointState_CREATE_FAILED,
  EndpointState_CREATING,
  EndpointState_DELETE_FAILED,
  EndpointState_DELETING,
  EndpointState_UPDATE_FAILED,
  EndpointState_UPDATING,
  EndpointState'
  #-}
