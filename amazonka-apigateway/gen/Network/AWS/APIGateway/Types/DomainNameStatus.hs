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
-- Module      : Network.AWS.APIGateway.Types.DomainNameStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.DomainNameStatus
  ( DomainNameStatus
      ( ..,
        DomainNameStatus_AVAILABLE,
        DomainNameStatus_PENDING,
        DomainNameStatus_PENDING_CERTIFICATE_REIMPORT,
        DomainNameStatus_PENDING_OWNERSHIP_VERIFICATION,
        DomainNameStatus_UPDATING
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype DomainNameStatus = DomainNameStatus'
  { fromDomainNameStatus ::
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

pattern DomainNameStatus_AVAILABLE :: DomainNameStatus
pattern DomainNameStatus_AVAILABLE = DomainNameStatus' "AVAILABLE"

pattern DomainNameStatus_PENDING :: DomainNameStatus
pattern DomainNameStatus_PENDING = DomainNameStatus' "PENDING"

pattern DomainNameStatus_PENDING_CERTIFICATE_REIMPORT :: DomainNameStatus
pattern DomainNameStatus_PENDING_CERTIFICATE_REIMPORT = DomainNameStatus' "PENDING_CERTIFICATE_REIMPORT"

pattern DomainNameStatus_PENDING_OWNERSHIP_VERIFICATION :: DomainNameStatus
pattern DomainNameStatus_PENDING_OWNERSHIP_VERIFICATION = DomainNameStatus' "PENDING_OWNERSHIP_VERIFICATION"

pattern DomainNameStatus_UPDATING :: DomainNameStatus
pattern DomainNameStatus_UPDATING = DomainNameStatus' "UPDATING"

{-# COMPLETE
  DomainNameStatus_AVAILABLE,
  DomainNameStatus_PENDING,
  DomainNameStatus_PENDING_CERTIFICATE_REIMPORT,
  DomainNameStatus_PENDING_OWNERSHIP_VERIFICATION,
  DomainNameStatus_UPDATING,
  DomainNameStatus'
  #-}
