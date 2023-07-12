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
-- Module      : Amazonka.OpenSearch.Types.VpcEndpointStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.VpcEndpointStatus
  ( VpcEndpointStatus
      ( ..,
        VpcEndpointStatus_ACTIVE,
        VpcEndpointStatus_CREATE_FAILED,
        VpcEndpointStatus_CREATING,
        VpcEndpointStatus_DELETE_FAILED,
        VpcEndpointStatus_DELETING,
        VpcEndpointStatus_UPDATE_FAILED,
        VpcEndpointStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype VpcEndpointStatus = VpcEndpointStatus'
  { fromVpcEndpointStatus ::
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

pattern VpcEndpointStatus_ACTIVE :: VpcEndpointStatus
pattern VpcEndpointStatus_ACTIVE = VpcEndpointStatus' "ACTIVE"

pattern VpcEndpointStatus_CREATE_FAILED :: VpcEndpointStatus
pattern VpcEndpointStatus_CREATE_FAILED = VpcEndpointStatus' "CREATE_FAILED"

pattern VpcEndpointStatus_CREATING :: VpcEndpointStatus
pattern VpcEndpointStatus_CREATING = VpcEndpointStatus' "CREATING"

pattern VpcEndpointStatus_DELETE_FAILED :: VpcEndpointStatus
pattern VpcEndpointStatus_DELETE_FAILED = VpcEndpointStatus' "DELETE_FAILED"

pattern VpcEndpointStatus_DELETING :: VpcEndpointStatus
pattern VpcEndpointStatus_DELETING = VpcEndpointStatus' "DELETING"

pattern VpcEndpointStatus_UPDATE_FAILED :: VpcEndpointStatus
pattern VpcEndpointStatus_UPDATE_FAILED = VpcEndpointStatus' "UPDATE_FAILED"

pattern VpcEndpointStatus_UPDATING :: VpcEndpointStatus
pattern VpcEndpointStatus_UPDATING = VpcEndpointStatus' "UPDATING"

{-# COMPLETE
  VpcEndpointStatus_ACTIVE,
  VpcEndpointStatus_CREATE_FAILED,
  VpcEndpointStatus_CREATING,
  VpcEndpointStatus_DELETE_FAILED,
  VpcEndpointStatus_DELETING,
  VpcEndpointStatus_UPDATE_FAILED,
  VpcEndpointStatus_UPDATING,
  VpcEndpointStatus'
  #-}
