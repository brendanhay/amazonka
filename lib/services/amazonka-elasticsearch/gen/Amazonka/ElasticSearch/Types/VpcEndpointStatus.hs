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
-- Module      : Amazonka.ElasticSearch.Types.VpcEndpointStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.VpcEndpointStatus
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
import qualified Amazonka.Prelude as Prelude

-- | Specifies the current status of the VPC endpoint:
--
-- -   CREATING: Indicates that the VPC endpoint is currently being
--     created.
-- -   CREATE_FAILED: Indicates that the VPC endpoint creation failed.
-- -   ACTIVE: Indicates that the VPC endpoint is currently active.
-- -   UPDATING: Indicates that the VPC endpoint is currently being
--     updated.
-- -   UPDATE_FAILED: Indicates that the VPC endpoint update failed.
-- -   DELETING: Indicates that the VPC endpoint is currently being
--     deleted.
-- -   DELETE_FAILED: Indicates that the VPC endpoint deletion failed.
newtype VpcEndpointStatus = VpcEndpointStatus'
  { fromVpcEndpointStatus ::
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
