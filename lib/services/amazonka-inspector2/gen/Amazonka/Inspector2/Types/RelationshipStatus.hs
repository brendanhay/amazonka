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
-- Module      : Amazonka.Inspector2.Types.RelationshipStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.RelationshipStatus
  ( RelationshipStatus
      ( ..,
        RelationshipStatus_ACCOUNT_SUSPENDED,
        RelationshipStatus_CANNOT_CREATE_DETECTOR_IN_ORG_MASTER,
        RelationshipStatus_CREATED,
        RelationshipStatus_DELETED,
        RelationshipStatus_DISABLED,
        RelationshipStatus_EMAIL_VERIFICATION_FAILED,
        RelationshipStatus_EMAIL_VERIFICATION_IN_PROGRESS,
        RelationshipStatus_ENABLED,
        RelationshipStatus_INVITED,
        RelationshipStatus_REGION_DISABLED,
        RelationshipStatus_REMOVED,
        RelationshipStatus_RESIGNED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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

pattern RelationshipStatus_ACCOUNT_SUSPENDED :: RelationshipStatus
pattern RelationshipStatus_ACCOUNT_SUSPENDED = RelationshipStatus' "ACCOUNT_SUSPENDED"

pattern RelationshipStatus_CANNOT_CREATE_DETECTOR_IN_ORG_MASTER :: RelationshipStatus
pattern RelationshipStatus_CANNOT_CREATE_DETECTOR_IN_ORG_MASTER = RelationshipStatus' "CANNOT_CREATE_DETECTOR_IN_ORG_MASTER"

pattern RelationshipStatus_CREATED :: RelationshipStatus
pattern RelationshipStatus_CREATED = RelationshipStatus' "CREATED"

pattern RelationshipStatus_DELETED :: RelationshipStatus
pattern RelationshipStatus_DELETED = RelationshipStatus' "DELETED"

pattern RelationshipStatus_DISABLED :: RelationshipStatus
pattern RelationshipStatus_DISABLED = RelationshipStatus' "DISABLED"

pattern RelationshipStatus_EMAIL_VERIFICATION_FAILED :: RelationshipStatus
pattern RelationshipStatus_EMAIL_VERIFICATION_FAILED = RelationshipStatus' "EMAIL_VERIFICATION_FAILED"

pattern RelationshipStatus_EMAIL_VERIFICATION_IN_PROGRESS :: RelationshipStatus
pattern RelationshipStatus_EMAIL_VERIFICATION_IN_PROGRESS = RelationshipStatus' "EMAIL_VERIFICATION_IN_PROGRESS"

pattern RelationshipStatus_ENABLED :: RelationshipStatus
pattern RelationshipStatus_ENABLED = RelationshipStatus' "ENABLED"

pattern RelationshipStatus_INVITED :: RelationshipStatus
pattern RelationshipStatus_INVITED = RelationshipStatus' "INVITED"

pattern RelationshipStatus_REGION_DISABLED :: RelationshipStatus
pattern RelationshipStatus_REGION_DISABLED = RelationshipStatus' "REGION_DISABLED"

pattern RelationshipStatus_REMOVED :: RelationshipStatus
pattern RelationshipStatus_REMOVED = RelationshipStatus' "REMOVED"

pattern RelationshipStatus_RESIGNED :: RelationshipStatus
pattern RelationshipStatus_RESIGNED = RelationshipStatus' "RESIGNED"

{-# COMPLETE
  RelationshipStatus_ACCOUNT_SUSPENDED,
  RelationshipStatus_CANNOT_CREATE_DETECTOR_IN_ORG_MASTER,
  RelationshipStatus_CREATED,
  RelationshipStatus_DELETED,
  RelationshipStatus_DISABLED,
  RelationshipStatus_EMAIL_VERIFICATION_FAILED,
  RelationshipStatus_EMAIL_VERIFICATION_IN_PROGRESS,
  RelationshipStatus_ENABLED,
  RelationshipStatus_INVITED,
  RelationshipStatus_REGION_DISABLED,
  RelationshipStatus_REMOVED,
  RelationshipStatus_RESIGNED,
  RelationshipStatus'
  #-}
