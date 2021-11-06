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
-- Module      : Amazonka.WorkLink.Types.DomainStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkLink.Types.DomainStatus
  ( DomainStatus
      ( ..,
        DomainStatus_ACTIVE,
        DomainStatus_ASSOCIATING,
        DomainStatus_DISASSOCIATED,
        DomainStatus_DISASSOCIATING,
        DomainStatus_FAILED_TO_ASSOCIATE,
        DomainStatus_FAILED_TO_DISASSOCIATE,
        DomainStatus_INACTIVE,
        DomainStatus_PENDING_VALIDATION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype DomainStatus = DomainStatus'
  { fromDomainStatus ::
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

pattern DomainStatus_ACTIVE :: DomainStatus
pattern DomainStatus_ACTIVE = DomainStatus' "ACTIVE"

pattern DomainStatus_ASSOCIATING :: DomainStatus
pattern DomainStatus_ASSOCIATING = DomainStatus' "ASSOCIATING"

pattern DomainStatus_DISASSOCIATED :: DomainStatus
pattern DomainStatus_DISASSOCIATED = DomainStatus' "DISASSOCIATED"

pattern DomainStatus_DISASSOCIATING :: DomainStatus
pattern DomainStatus_DISASSOCIATING = DomainStatus' "DISASSOCIATING"

pattern DomainStatus_FAILED_TO_ASSOCIATE :: DomainStatus
pattern DomainStatus_FAILED_TO_ASSOCIATE = DomainStatus' "FAILED_TO_ASSOCIATE"

pattern DomainStatus_FAILED_TO_DISASSOCIATE :: DomainStatus
pattern DomainStatus_FAILED_TO_DISASSOCIATE = DomainStatus' "FAILED_TO_DISASSOCIATE"

pattern DomainStatus_INACTIVE :: DomainStatus
pattern DomainStatus_INACTIVE = DomainStatus' "INACTIVE"

pattern DomainStatus_PENDING_VALIDATION :: DomainStatus
pattern DomainStatus_PENDING_VALIDATION = DomainStatus' "PENDING_VALIDATION"

{-# COMPLETE
  DomainStatus_ACTIVE,
  DomainStatus_ASSOCIATING,
  DomainStatus_DISASSOCIATED,
  DomainStatus_DISASSOCIATING,
  DomainStatus_FAILED_TO_ASSOCIATE,
  DomainStatus_FAILED_TO_DISASSOCIATE,
  DomainStatus_INACTIVE,
  DomainStatus_PENDING_VALIDATION,
  DomainStatus'
  #-}
