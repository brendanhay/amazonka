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
-- Module      : Amazonka.Comprehend.Types.EndpointStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.EndpointStatus
  ( EndpointStatus
      ( ..,
        EndpointStatus_CREATING,
        EndpointStatus_DELETING,
        EndpointStatus_FAILED,
        EndpointStatus_IN_SERVICE,
        EndpointStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype EndpointStatus = EndpointStatus'
  { fromEndpointStatus ::
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

pattern EndpointStatus_CREATING :: EndpointStatus
pattern EndpointStatus_CREATING = EndpointStatus' "CREATING"

pattern EndpointStatus_DELETING :: EndpointStatus
pattern EndpointStatus_DELETING = EndpointStatus' "DELETING"

pattern EndpointStatus_FAILED :: EndpointStatus
pattern EndpointStatus_FAILED = EndpointStatus' "FAILED"

pattern EndpointStatus_IN_SERVICE :: EndpointStatus
pattern EndpointStatus_IN_SERVICE = EndpointStatus' "IN_SERVICE"

pattern EndpointStatus_UPDATING :: EndpointStatus
pattern EndpointStatus_UPDATING = EndpointStatus' "UPDATING"

{-# COMPLETE
  EndpointStatus_CREATING,
  EndpointStatus_DELETING,
  EndpointStatus_FAILED,
  EndpointStatus_IN_SERVICE,
  EndpointStatus_UPDATING,
  EndpointStatus'
  #-}
