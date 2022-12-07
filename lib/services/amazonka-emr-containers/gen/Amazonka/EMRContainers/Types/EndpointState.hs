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
-- Module      : Amazonka.EMRContainers.Types.EndpointState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRContainers.Types.EndpointState
  ( EndpointState
      ( ..,
        EndpointState_ACTIVE,
        EndpointState_CREATING,
        EndpointState_TERMINATED,
        EndpointState_TERMINATED_WITH_ERRORS,
        EndpointState_TERMINATING
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

pattern EndpointState_CREATING :: EndpointState
pattern EndpointState_CREATING = EndpointState' "CREATING"

pattern EndpointState_TERMINATED :: EndpointState
pattern EndpointState_TERMINATED = EndpointState' "TERMINATED"

pattern EndpointState_TERMINATED_WITH_ERRORS :: EndpointState
pattern EndpointState_TERMINATED_WITH_ERRORS = EndpointState' "TERMINATED_WITH_ERRORS"

pattern EndpointState_TERMINATING :: EndpointState
pattern EndpointState_TERMINATING = EndpointState' "TERMINATING"

{-# COMPLETE
  EndpointState_ACTIVE,
  EndpointState_CREATING,
  EndpointState_TERMINATED,
  EndpointState_TERMINATED_WITH_ERRORS,
  EndpointState_TERMINATING,
  EndpointState'
  #-}
