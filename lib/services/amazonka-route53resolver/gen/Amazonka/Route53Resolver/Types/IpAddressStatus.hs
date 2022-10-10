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
-- Module      : Amazonka.Route53Resolver.Types.IpAddressStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Resolver.Types.IpAddressStatus
  ( IpAddressStatus
      ( ..,
        IpAddressStatus_ATTACHED,
        IpAddressStatus_ATTACHING,
        IpAddressStatus_CREATING,
        IpAddressStatus_DELETE_FAILED_FAS_EXPIRED,
        IpAddressStatus_DELETING,
        IpAddressStatus_DETACHING,
        IpAddressStatus_FAILED_CREATION,
        IpAddressStatus_FAILED_RESOURCE_GONE,
        IpAddressStatus_REMAP_ATTACHING,
        IpAddressStatus_REMAP_DETACHING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype IpAddressStatus = IpAddressStatus'
  { fromIpAddressStatus ::
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

pattern IpAddressStatus_ATTACHED :: IpAddressStatus
pattern IpAddressStatus_ATTACHED = IpAddressStatus' "ATTACHED"

pattern IpAddressStatus_ATTACHING :: IpAddressStatus
pattern IpAddressStatus_ATTACHING = IpAddressStatus' "ATTACHING"

pattern IpAddressStatus_CREATING :: IpAddressStatus
pattern IpAddressStatus_CREATING = IpAddressStatus' "CREATING"

pattern IpAddressStatus_DELETE_FAILED_FAS_EXPIRED :: IpAddressStatus
pattern IpAddressStatus_DELETE_FAILED_FAS_EXPIRED = IpAddressStatus' "DELETE_FAILED_FAS_EXPIRED"

pattern IpAddressStatus_DELETING :: IpAddressStatus
pattern IpAddressStatus_DELETING = IpAddressStatus' "DELETING"

pattern IpAddressStatus_DETACHING :: IpAddressStatus
pattern IpAddressStatus_DETACHING = IpAddressStatus' "DETACHING"

pattern IpAddressStatus_FAILED_CREATION :: IpAddressStatus
pattern IpAddressStatus_FAILED_CREATION = IpAddressStatus' "FAILED_CREATION"

pattern IpAddressStatus_FAILED_RESOURCE_GONE :: IpAddressStatus
pattern IpAddressStatus_FAILED_RESOURCE_GONE = IpAddressStatus' "FAILED_RESOURCE_GONE"

pattern IpAddressStatus_REMAP_ATTACHING :: IpAddressStatus
pattern IpAddressStatus_REMAP_ATTACHING = IpAddressStatus' "REMAP_ATTACHING"

pattern IpAddressStatus_REMAP_DETACHING :: IpAddressStatus
pattern IpAddressStatus_REMAP_DETACHING = IpAddressStatus' "REMAP_DETACHING"

{-# COMPLETE
  IpAddressStatus_ATTACHED,
  IpAddressStatus_ATTACHING,
  IpAddressStatus_CREATING,
  IpAddressStatus_DELETE_FAILED_FAS_EXPIRED,
  IpAddressStatus_DELETING,
  IpAddressStatus_DETACHING,
  IpAddressStatus_FAILED_CREATION,
  IpAddressStatus_FAILED_RESOURCE_GONE,
  IpAddressStatus_REMAP_ATTACHING,
  IpAddressStatus_REMAP_DETACHING,
  IpAddressStatus'
  #-}
