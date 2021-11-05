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
-- Module      : Network.AWS.GroundStation.Types.EndpointStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GroundStation.Types.EndpointStatus
  ( EndpointStatus
      ( ..,
        EndpointStatus_Created,
        EndpointStatus_Creating,
        EndpointStatus_Deleted,
        EndpointStatus_Deleting,
        EndpointStatus_Failed
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

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

pattern EndpointStatus_Created :: EndpointStatus
pattern EndpointStatus_Created = EndpointStatus' "created"

pattern EndpointStatus_Creating :: EndpointStatus
pattern EndpointStatus_Creating = EndpointStatus' "creating"

pattern EndpointStatus_Deleted :: EndpointStatus
pattern EndpointStatus_Deleted = EndpointStatus' "deleted"

pattern EndpointStatus_Deleting :: EndpointStatus
pattern EndpointStatus_Deleting = EndpointStatus' "deleting"

pattern EndpointStatus_Failed :: EndpointStatus
pattern EndpointStatus_Failed = EndpointStatus' "failed"

{-# COMPLETE
  EndpointStatus_Created,
  EndpointStatus_Creating,
  EndpointStatus_Deleted,
  EndpointStatus_Deleting,
  EndpointStatus_Failed,
  EndpointStatus'
  #-}
