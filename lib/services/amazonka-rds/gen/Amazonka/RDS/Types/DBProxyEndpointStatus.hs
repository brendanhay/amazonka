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
-- Module      : Amazonka.RDS.Types.DBProxyEndpointStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.DBProxyEndpointStatus
  ( DBProxyEndpointStatus
      ( ..,
        DBProxyEndpointStatus_Available,
        DBProxyEndpointStatus_Creating,
        DBProxyEndpointStatus_Deleting,
        DBProxyEndpointStatus_Incompatible_network,
        DBProxyEndpointStatus_Insufficient_resource_limits,
        DBProxyEndpointStatus_Modifying
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DBProxyEndpointStatus = DBProxyEndpointStatus'
  { fromDBProxyEndpointStatus ::
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

pattern DBProxyEndpointStatus_Available :: DBProxyEndpointStatus
pattern DBProxyEndpointStatus_Available = DBProxyEndpointStatus' "available"

pattern DBProxyEndpointStatus_Creating :: DBProxyEndpointStatus
pattern DBProxyEndpointStatus_Creating = DBProxyEndpointStatus' "creating"

pattern DBProxyEndpointStatus_Deleting :: DBProxyEndpointStatus
pattern DBProxyEndpointStatus_Deleting = DBProxyEndpointStatus' "deleting"

pattern DBProxyEndpointStatus_Incompatible_network :: DBProxyEndpointStatus
pattern DBProxyEndpointStatus_Incompatible_network = DBProxyEndpointStatus' "incompatible-network"

pattern DBProxyEndpointStatus_Insufficient_resource_limits :: DBProxyEndpointStatus
pattern DBProxyEndpointStatus_Insufficient_resource_limits = DBProxyEndpointStatus' "insufficient-resource-limits"

pattern DBProxyEndpointStatus_Modifying :: DBProxyEndpointStatus
pattern DBProxyEndpointStatus_Modifying = DBProxyEndpointStatus' "modifying"

{-# COMPLETE
  DBProxyEndpointStatus_Available,
  DBProxyEndpointStatus_Creating,
  DBProxyEndpointStatus_Deleting,
  DBProxyEndpointStatus_Incompatible_network,
  DBProxyEndpointStatus_Insufficient_resource_limits,
  DBProxyEndpointStatus_Modifying,
  DBProxyEndpointStatus'
  #-}
