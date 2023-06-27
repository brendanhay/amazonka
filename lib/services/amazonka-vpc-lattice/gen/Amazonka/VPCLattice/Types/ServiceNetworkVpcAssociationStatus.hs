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
-- Module      : Amazonka.VPCLattice.Types.ServiceNetworkVpcAssociationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.ServiceNetworkVpcAssociationStatus
  ( ServiceNetworkVpcAssociationStatus
      ( ..,
        ServiceNetworkVpcAssociationStatus_ACTIVE,
        ServiceNetworkVpcAssociationStatus_CREATE_FAILED,
        ServiceNetworkVpcAssociationStatus_CREATE_IN_PROGRESS,
        ServiceNetworkVpcAssociationStatus_DELETE_FAILED,
        ServiceNetworkVpcAssociationStatus_DELETE_IN_PROGRESS,
        ServiceNetworkVpcAssociationStatus_UPDATE_FAILED,
        ServiceNetworkVpcAssociationStatus_UPDATE_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ServiceNetworkVpcAssociationStatus = ServiceNetworkVpcAssociationStatus'
  { fromServiceNetworkVpcAssociationStatus ::
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

pattern ServiceNetworkVpcAssociationStatus_ACTIVE :: ServiceNetworkVpcAssociationStatus
pattern ServiceNetworkVpcAssociationStatus_ACTIVE = ServiceNetworkVpcAssociationStatus' "ACTIVE"

pattern ServiceNetworkVpcAssociationStatus_CREATE_FAILED :: ServiceNetworkVpcAssociationStatus
pattern ServiceNetworkVpcAssociationStatus_CREATE_FAILED = ServiceNetworkVpcAssociationStatus' "CREATE_FAILED"

pattern ServiceNetworkVpcAssociationStatus_CREATE_IN_PROGRESS :: ServiceNetworkVpcAssociationStatus
pattern ServiceNetworkVpcAssociationStatus_CREATE_IN_PROGRESS = ServiceNetworkVpcAssociationStatus' "CREATE_IN_PROGRESS"

pattern ServiceNetworkVpcAssociationStatus_DELETE_FAILED :: ServiceNetworkVpcAssociationStatus
pattern ServiceNetworkVpcAssociationStatus_DELETE_FAILED = ServiceNetworkVpcAssociationStatus' "DELETE_FAILED"

pattern ServiceNetworkVpcAssociationStatus_DELETE_IN_PROGRESS :: ServiceNetworkVpcAssociationStatus
pattern ServiceNetworkVpcAssociationStatus_DELETE_IN_PROGRESS = ServiceNetworkVpcAssociationStatus' "DELETE_IN_PROGRESS"

pattern ServiceNetworkVpcAssociationStatus_UPDATE_FAILED :: ServiceNetworkVpcAssociationStatus
pattern ServiceNetworkVpcAssociationStatus_UPDATE_FAILED = ServiceNetworkVpcAssociationStatus' "UPDATE_FAILED"

pattern ServiceNetworkVpcAssociationStatus_UPDATE_IN_PROGRESS :: ServiceNetworkVpcAssociationStatus
pattern ServiceNetworkVpcAssociationStatus_UPDATE_IN_PROGRESS = ServiceNetworkVpcAssociationStatus' "UPDATE_IN_PROGRESS"

{-# COMPLETE
  ServiceNetworkVpcAssociationStatus_ACTIVE,
  ServiceNetworkVpcAssociationStatus_CREATE_FAILED,
  ServiceNetworkVpcAssociationStatus_CREATE_IN_PROGRESS,
  ServiceNetworkVpcAssociationStatus_DELETE_FAILED,
  ServiceNetworkVpcAssociationStatus_DELETE_IN_PROGRESS,
  ServiceNetworkVpcAssociationStatus_UPDATE_FAILED,
  ServiceNetworkVpcAssociationStatus_UPDATE_IN_PROGRESS,
  ServiceNetworkVpcAssociationStatus'
  #-}
