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
-- Module      : Amazonka.VPCLattice.Types.ServiceStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.ServiceStatus
  ( ServiceStatus
      ( ..,
        ServiceStatus_ACTIVE,
        ServiceStatus_CREATE_FAILED,
        ServiceStatus_CREATE_IN_PROGRESS,
        ServiceStatus_DELETE_FAILED,
        ServiceStatus_DELETE_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ServiceStatus = ServiceStatus'
  { fromServiceStatus ::
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

pattern ServiceStatus_ACTIVE :: ServiceStatus
pattern ServiceStatus_ACTIVE = ServiceStatus' "ACTIVE"

pattern ServiceStatus_CREATE_FAILED :: ServiceStatus
pattern ServiceStatus_CREATE_FAILED = ServiceStatus' "CREATE_FAILED"

pattern ServiceStatus_CREATE_IN_PROGRESS :: ServiceStatus
pattern ServiceStatus_CREATE_IN_PROGRESS = ServiceStatus' "CREATE_IN_PROGRESS"

pattern ServiceStatus_DELETE_FAILED :: ServiceStatus
pattern ServiceStatus_DELETE_FAILED = ServiceStatus' "DELETE_FAILED"

pattern ServiceStatus_DELETE_IN_PROGRESS :: ServiceStatus
pattern ServiceStatus_DELETE_IN_PROGRESS = ServiceStatus' "DELETE_IN_PROGRESS"

{-# COMPLETE
  ServiceStatus_ACTIVE,
  ServiceStatus_CREATE_FAILED,
  ServiceStatus_CREATE_IN_PROGRESS,
  ServiceStatus_DELETE_FAILED,
  ServiceStatus_DELETE_IN_PROGRESS,
  ServiceStatus'
  #-}
