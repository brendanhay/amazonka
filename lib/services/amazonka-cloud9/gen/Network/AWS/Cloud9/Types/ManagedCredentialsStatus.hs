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
-- Module      : Network.AWS.Cloud9.Types.ManagedCredentialsStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Cloud9.Types.ManagedCredentialsStatus
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ManagedCredentialsStatus = ManagedCredentialsStatus'
  { fromManagedCredentialsStatus ::
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
