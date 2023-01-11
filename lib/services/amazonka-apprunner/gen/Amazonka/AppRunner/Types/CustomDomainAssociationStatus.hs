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
-- Module      : Amazonka.AppRunner.Types.CustomDomainAssociationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.CustomDomainAssociationStatus
  ( CustomDomainAssociationStatus
      ( ..,
        CustomDomainAssociationStatus_ACTIVE,
        CustomDomainAssociationStatus_BINDING_CERTIFICATE,
        CustomDomainAssociationStatus_CREATE_FAILED,
        CustomDomainAssociationStatus_CREATING,
        CustomDomainAssociationStatus_DELETE_FAILED,
        CustomDomainAssociationStatus_DELETING,
        CustomDomainAssociationStatus_PENDING_CERTIFICATE_DNS_VALIDATION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CustomDomainAssociationStatus = CustomDomainAssociationStatus'
  { fromCustomDomainAssociationStatus ::
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

pattern CustomDomainAssociationStatus_ACTIVE :: CustomDomainAssociationStatus
pattern CustomDomainAssociationStatus_ACTIVE = CustomDomainAssociationStatus' "ACTIVE"

pattern CustomDomainAssociationStatus_BINDING_CERTIFICATE :: CustomDomainAssociationStatus
pattern CustomDomainAssociationStatus_BINDING_CERTIFICATE = CustomDomainAssociationStatus' "BINDING_CERTIFICATE"

pattern CustomDomainAssociationStatus_CREATE_FAILED :: CustomDomainAssociationStatus
pattern CustomDomainAssociationStatus_CREATE_FAILED = CustomDomainAssociationStatus' "CREATE_FAILED"

pattern CustomDomainAssociationStatus_CREATING :: CustomDomainAssociationStatus
pattern CustomDomainAssociationStatus_CREATING = CustomDomainAssociationStatus' "CREATING"

pattern CustomDomainAssociationStatus_DELETE_FAILED :: CustomDomainAssociationStatus
pattern CustomDomainAssociationStatus_DELETE_FAILED = CustomDomainAssociationStatus' "DELETE_FAILED"

pattern CustomDomainAssociationStatus_DELETING :: CustomDomainAssociationStatus
pattern CustomDomainAssociationStatus_DELETING = CustomDomainAssociationStatus' "DELETING"

pattern CustomDomainAssociationStatus_PENDING_CERTIFICATE_DNS_VALIDATION :: CustomDomainAssociationStatus
pattern CustomDomainAssociationStatus_PENDING_CERTIFICATE_DNS_VALIDATION = CustomDomainAssociationStatus' "PENDING_CERTIFICATE_DNS_VALIDATION"

{-# COMPLETE
  CustomDomainAssociationStatus_ACTIVE,
  CustomDomainAssociationStatus_BINDING_CERTIFICATE,
  CustomDomainAssociationStatus_CREATE_FAILED,
  CustomDomainAssociationStatus_CREATING,
  CustomDomainAssociationStatus_DELETE_FAILED,
  CustomDomainAssociationStatus_DELETING,
  CustomDomainAssociationStatus_PENDING_CERTIFICATE_DNS_VALIDATION,
  CustomDomainAssociationStatus'
  #-}
