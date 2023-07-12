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
-- Module      : Amazonka.Amplify.Types.DomainStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Amplify.Types.DomainStatus
  ( DomainStatus
      ( ..,
        DomainStatus_AVAILABLE,
        DomainStatus_CREATING,
        DomainStatus_FAILED,
        DomainStatus_IN_PROGRESS,
        DomainStatus_PENDING_DEPLOYMENT,
        DomainStatus_PENDING_VERIFICATION,
        DomainStatus_REQUESTING_CERTIFICATE,
        DomainStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DomainStatus = DomainStatus'
  { fromDomainStatus ::
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

pattern DomainStatus_AVAILABLE :: DomainStatus
pattern DomainStatus_AVAILABLE = DomainStatus' "AVAILABLE"

pattern DomainStatus_CREATING :: DomainStatus
pattern DomainStatus_CREATING = DomainStatus' "CREATING"

pattern DomainStatus_FAILED :: DomainStatus
pattern DomainStatus_FAILED = DomainStatus' "FAILED"

pattern DomainStatus_IN_PROGRESS :: DomainStatus
pattern DomainStatus_IN_PROGRESS = DomainStatus' "IN_PROGRESS"

pattern DomainStatus_PENDING_DEPLOYMENT :: DomainStatus
pattern DomainStatus_PENDING_DEPLOYMENT = DomainStatus' "PENDING_DEPLOYMENT"

pattern DomainStatus_PENDING_VERIFICATION :: DomainStatus
pattern DomainStatus_PENDING_VERIFICATION = DomainStatus' "PENDING_VERIFICATION"

pattern DomainStatus_REQUESTING_CERTIFICATE :: DomainStatus
pattern DomainStatus_REQUESTING_CERTIFICATE = DomainStatus' "REQUESTING_CERTIFICATE"

pattern DomainStatus_UPDATING :: DomainStatus
pattern DomainStatus_UPDATING = DomainStatus' "UPDATING"

{-# COMPLETE
  DomainStatus_AVAILABLE,
  DomainStatus_CREATING,
  DomainStatus_FAILED,
  DomainStatus_IN_PROGRESS,
  DomainStatus_PENDING_DEPLOYMENT,
  DomainStatus_PENDING_VERIFICATION,
  DomainStatus_REQUESTING_CERTIFICATE,
  DomainStatus_UPDATING,
  DomainStatus'
  #-}
