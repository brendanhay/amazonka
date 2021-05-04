{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.DomainStatusType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.DomainStatusType
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

import qualified Network.AWS.Prelude as Prelude

newtype DomainStatusType = DomainStatusType'
  { fromDomainStatusType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
