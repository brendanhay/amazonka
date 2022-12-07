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
-- Module      : Amazonka.MacieV2.Types.RelationshipStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.RelationshipStatus
  ( RelationshipStatus
      ( ..,
        RelationshipStatus_AccountSuspended,
        RelationshipStatus_Created,
        RelationshipStatus_EmailVerificationFailed,
        RelationshipStatus_EmailVerificationInProgress,
        RelationshipStatus_Enabled,
        RelationshipStatus_Invited,
        RelationshipStatus_Paused,
        RelationshipStatus_RegionDisabled,
        RelationshipStatus_Removed,
        RelationshipStatus_Resigned
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The current status of the relationship between an account and an
-- associated Amazon Macie administrator account. Possible values are:
newtype RelationshipStatus = RelationshipStatus'
  { fromRelationshipStatus ::
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

pattern RelationshipStatus_AccountSuspended :: RelationshipStatus
pattern RelationshipStatus_AccountSuspended = RelationshipStatus' "AccountSuspended"

pattern RelationshipStatus_Created :: RelationshipStatus
pattern RelationshipStatus_Created = RelationshipStatus' "Created"

pattern RelationshipStatus_EmailVerificationFailed :: RelationshipStatus
pattern RelationshipStatus_EmailVerificationFailed = RelationshipStatus' "EmailVerificationFailed"

pattern RelationshipStatus_EmailVerificationInProgress :: RelationshipStatus
pattern RelationshipStatus_EmailVerificationInProgress = RelationshipStatus' "EmailVerificationInProgress"

pattern RelationshipStatus_Enabled :: RelationshipStatus
pattern RelationshipStatus_Enabled = RelationshipStatus' "Enabled"

pattern RelationshipStatus_Invited :: RelationshipStatus
pattern RelationshipStatus_Invited = RelationshipStatus' "Invited"

pattern RelationshipStatus_Paused :: RelationshipStatus
pattern RelationshipStatus_Paused = RelationshipStatus' "Paused"

pattern RelationshipStatus_RegionDisabled :: RelationshipStatus
pattern RelationshipStatus_RegionDisabled = RelationshipStatus' "RegionDisabled"

pattern RelationshipStatus_Removed :: RelationshipStatus
pattern RelationshipStatus_Removed = RelationshipStatus' "Removed"

pattern RelationshipStatus_Resigned :: RelationshipStatus
pattern RelationshipStatus_Resigned = RelationshipStatus' "Resigned"

{-# COMPLETE
  RelationshipStatus_AccountSuspended,
  RelationshipStatus_Created,
  RelationshipStatus_EmailVerificationFailed,
  RelationshipStatus_EmailVerificationInProgress,
  RelationshipStatus_Enabled,
  RelationshipStatus_Invited,
  RelationshipStatus_Paused,
  RelationshipStatus_RegionDisabled,
  RelationshipStatus_Removed,
  RelationshipStatus_Resigned,
  RelationshipStatus'
  #-}
