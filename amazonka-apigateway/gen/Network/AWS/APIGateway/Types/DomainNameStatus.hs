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
        DomainNameStatus_UPDATING
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype DomainNameStatus = DomainNameStatus'
  { fromDomainNameStatus ::
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

pattern DomainNameStatus_AVAILABLE :: DomainNameStatus
pattern DomainNameStatus_AVAILABLE = DomainNameStatus' "AVAILABLE"

pattern DomainNameStatus_PENDING :: DomainNameStatus
pattern DomainNameStatus_PENDING = DomainNameStatus' "PENDING"

pattern DomainNameStatus_UPDATING :: DomainNameStatus
pattern DomainNameStatus_UPDATING = DomainNameStatus' "UPDATING"

{-# COMPLETE
  DomainNameStatus_AVAILABLE,
  DomainNameStatus_PENDING,
  DomainNameStatus_UPDATING,
  DomainNameStatus'
  #-}
