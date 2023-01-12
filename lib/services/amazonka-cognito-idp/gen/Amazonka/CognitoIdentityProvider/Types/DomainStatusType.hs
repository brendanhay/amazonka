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
-- Module      : Amazonka.CognitoIdentityProvider.Types.DomainStatusType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.DomainStatusType
  ( DomainStatusType
      ( ..,
        DomainStatusType_ACTIVE,
        DomainStatusType_CREATING,
        DomainStatusType_DELETING,
        DomainStatusType_FAILED,
        DomainStatusType_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DomainStatusType = DomainStatusType'
  { fromDomainStatusType ::
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

pattern DomainStatusType_ACTIVE :: DomainStatusType
pattern DomainStatusType_ACTIVE = DomainStatusType' "ACTIVE"

pattern DomainStatusType_CREATING :: DomainStatusType
pattern DomainStatusType_CREATING = DomainStatusType' "CREATING"

pattern DomainStatusType_DELETING :: DomainStatusType
pattern DomainStatusType_DELETING = DomainStatusType' "DELETING"

pattern DomainStatusType_FAILED :: DomainStatusType
pattern DomainStatusType_FAILED = DomainStatusType' "FAILED"

pattern DomainStatusType_UPDATING :: DomainStatusType
pattern DomainStatusType_UPDATING = DomainStatusType' "UPDATING"

{-# COMPLETE
  DomainStatusType_ACTIVE,
  DomainStatusType_CREATING,
  DomainStatusType_DELETING,
  DomainStatusType_FAILED,
  DomainStatusType_UPDATING,
  DomainStatusType'
  #-}
