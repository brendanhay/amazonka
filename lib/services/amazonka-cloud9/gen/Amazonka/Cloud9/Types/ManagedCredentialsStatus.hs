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
-- Module      : Amazonka.Cloud9.Types.ManagedCredentialsStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Cloud9.Types.ManagedCredentialsStatus
  ( ManagedCredentialsStatus
      ( ..,
        ManagedCredentialsStatus_DISABLED_BY_COLLABORATOR,
        ManagedCredentialsStatus_DISABLED_BY_DEFAULT,
        ManagedCredentialsStatus_DISABLED_BY_OWNER,
        ManagedCredentialsStatus_ENABLED_BY_OWNER,
        ManagedCredentialsStatus_ENABLED_ON_CREATE,
        ManagedCredentialsStatus_FAILED_REMOVAL_BY_COLLABORATOR,
        ManagedCredentialsStatus_FAILED_REMOVAL_BY_OWNER,
        ManagedCredentialsStatus_PENDING_REMOVAL_BY_COLLABORATOR,
        ManagedCredentialsStatus_PENDING_REMOVAL_BY_OWNER,
        ManagedCredentialsStatus_PENDING_START_REMOVAL_BY_COLLABORATOR,
        ManagedCredentialsStatus_PENDING_START_REMOVAL_BY_OWNER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ManagedCredentialsStatus = ManagedCredentialsStatus'
  { fromManagedCredentialsStatus ::
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

pattern ManagedCredentialsStatus_DISABLED_BY_COLLABORATOR :: ManagedCredentialsStatus
pattern ManagedCredentialsStatus_DISABLED_BY_COLLABORATOR = ManagedCredentialsStatus' "DISABLED_BY_COLLABORATOR"

pattern ManagedCredentialsStatus_DISABLED_BY_DEFAULT :: ManagedCredentialsStatus
pattern ManagedCredentialsStatus_DISABLED_BY_DEFAULT = ManagedCredentialsStatus' "DISABLED_BY_DEFAULT"

pattern ManagedCredentialsStatus_DISABLED_BY_OWNER :: ManagedCredentialsStatus
pattern ManagedCredentialsStatus_DISABLED_BY_OWNER = ManagedCredentialsStatus' "DISABLED_BY_OWNER"

pattern ManagedCredentialsStatus_ENABLED_BY_OWNER :: ManagedCredentialsStatus
pattern ManagedCredentialsStatus_ENABLED_BY_OWNER = ManagedCredentialsStatus' "ENABLED_BY_OWNER"

pattern ManagedCredentialsStatus_ENABLED_ON_CREATE :: ManagedCredentialsStatus
pattern ManagedCredentialsStatus_ENABLED_ON_CREATE = ManagedCredentialsStatus' "ENABLED_ON_CREATE"

pattern ManagedCredentialsStatus_FAILED_REMOVAL_BY_COLLABORATOR :: ManagedCredentialsStatus
pattern ManagedCredentialsStatus_FAILED_REMOVAL_BY_COLLABORATOR = ManagedCredentialsStatus' "FAILED_REMOVAL_BY_COLLABORATOR"

pattern ManagedCredentialsStatus_FAILED_REMOVAL_BY_OWNER :: ManagedCredentialsStatus
pattern ManagedCredentialsStatus_FAILED_REMOVAL_BY_OWNER = ManagedCredentialsStatus' "FAILED_REMOVAL_BY_OWNER"

pattern ManagedCredentialsStatus_PENDING_REMOVAL_BY_COLLABORATOR :: ManagedCredentialsStatus
pattern ManagedCredentialsStatus_PENDING_REMOVAL_BY_COLLABORATOR = ManagedCredentialsStatus' "PENDING_REMOVAL_BY_COLLABORATOR"

pattern ManagedCredentialsStatus_PENDING_REMOVAL_BY_OWNER :: ManagedCredentialsStatus
pattern ManagedCredentialsStatus_PENDING_REMOVAL_BY_OWNER = ManagedCredentialsStatus' "PENDING_REMOVAL_BY_OWNER"

pattern ManagedCredentialsStatus_PENDING_START_REMOVAL_BY_COLLABORATOR :: ManagedCredentialsStatus
pattern ManagedCredentialsStatus_PENDING_START_REMOVAL_BY_COLLABORATOR = ManagedCredentialsStatus' "PENDING_START_REMOVAL_BY_COLLABORATOR"

pattern ManagedCredentialsStatus_PENDING_START_REMOVAL_BY_OWNER :: ManagedCredentialsStatus
pattern ManagedCredentialsStatus_PENDING_START_REMOVAL_BY_OWNER = ManagedCredentialsStatus' "PENDING_START_REMOVAL_BY_OWNER"

{-# COMPLETE
  ManagedCredentialsStatus_DISABLED_BY_COLLABORATOR,
  ManagedCredentialsStatus_DISABLED_BY_DEFAULT,
  ManagedCredentialsStatus_DISABLED_BY_OWNER,
  ManagedCredentialsStatus_ENABLED_BY_OWNER,
  ManagedCredentialsStatus_ENABLED_ON_CREATE,
  ManagedCredentialsStatus_FAILED_REMOVAL_BY_COLLABORATOR,
  ManagedCredentialsStatus_FAILED_REMOVAL_BY_OWNER,
  ManagedCredentialsStatus_PENDING_REMOVAL_BY_COLLABORATOR,
  ManagedCredentialsStatus_PENDING_REMOVAL_BY_OWNER,
  ManagedCredentialsStatus_PENDING_START_REMOVAL_BY_COLLABORATOR,
  ManagedCredentialsStatus_PENDING_START_REMOVAL_BY_OWNER,
  ManagedCredentialsStatus'
  #-}
