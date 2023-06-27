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
-- Module      : Amazonka.VPCLattice.Types.ServiceNetworkServiceAssociationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.ServiceNetworkServiceAssociationStatus
  ( ServiceNetworkServiceAssociationStatus
      ( ..,
        ServiceNetworkServiceAssociationStatus_ACTIVE,
        ServiceNetworkServiceAssociationStatus_CREATE_FAILED,
        ServiceNetworkServiceAssociationStatus_CREATE_IN_PROGRESS,
        ServiceNetworkServiceAssociationStatus_DELETE_FAILED,
        ServiceNetworkServiceAssociationStatus_DELETE_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ServiceNetworkServiceAssociationStatus = ServiceNetworkServiceAssociationStatus'
  { fromServiceNetworkServiceAssociationStatus ::
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

pattern ServiceNetworkServiceAssociationStatus_ACTIVE :: ServiceNetworkServiceAssociationStatus
pattern ServiceNetworkServiceAssociationStatus_ACTIVE = ServiceNetworkServiceAssociationStatus' "ACTIVE"

pattern ServiceNetworkServiceAssociationStatus_CREATE_FAILED :: ServiceNetworkServiceAssociationStatus
pattern ServiceNetworkServiceAssociationStatus_CREATE_FAILED = ServiceNetworkServiceAssociationStatus' "CREATE_FAILED"

pattern ServiceNetworkServiceAssociationStatus_CREATE_IN_PROGRESS :: ServiceNetworkServiceAssociationStatus
pattern ServiceNetworkServiceAssociationStatus_CREATE_IN_PROGRESS = ServiceNetworkServiceAssociationStatus' "CREATE_IN_PROGRESS"

pattern ServiceNetworkServiceAssociationStatus_DELETE_FAILED :: ServiceNetworkServiceAssociationStatus
pattern ServiceNetworkServiceAssociationStatus_DELETE_FAILED = ServiceNetworkServiceAssociationStatus' "DELETE_FAILED"

pattern ServiceNetworkServiceAssociationStatus_DELETE_IN_PROGRESS :: ServiceNetworkServiceAssociationStatus
pattern ServiceNetworkServiceAssociationStatus_DELETE_IN_PROGRESS = ServiceNetworkServiceAssociationStatus' "DELETE_IN_PROGRESS"

{-# COMPLETE
  ServiceNetworkServiceAssociationStatus_ACTIVE,
  ServiceNetworkServiceAssociationStatus_CREATE_FAILED,
  ServiceNetworkServiceAssociationStatus_CREATE_IN_PROGRESS,
  ServiceNetworkServiceAssociationStatus_DELETE_FAILED,
  ServiceNetworkServiceAssociationStatus_DELETE_IN_PROGRESS,
  ServiceNetworkServiceAssociationStatus'
  #-}
