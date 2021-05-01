{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EndpointStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EndpointStatus
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

import qualified Network.AWS.Prelude as Prelude

newtype EndpointStatus = EndpointStatus'
  { fromEndpointStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
