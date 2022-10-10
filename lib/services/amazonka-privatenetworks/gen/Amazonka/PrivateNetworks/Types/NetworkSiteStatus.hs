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
-- Module      : Amazonka.PrivateNetworks.Types.NetworkSiteStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PrivateNetworks.Types.NetworkSiteStatus
  ( NetworkSiteStatus
      ( ..,
        NetworkSiteStatus_AVAILABLE,
        NetworkSiteStatus_CREATED,
        NetworkSiteStatus_DELETED,
        NetworkSiteStatus_DEPROVISIONING,
        NetworkSiteStatus_PROVISIONING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype NetworkSiteStatus = NetworkSiteStatus'
  { fromNetworkSiteStatus ::
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

pattern NetworkSiteStatus_AVAILABLE :: NetworkSiteStatus
pattern NetworkSiteStatus_AVAILABLE = NetworkSiteStatus' "AVAILABLE"

pattern NetworkSiteStatus_CREATED :: NetworkSiteStatus
pattern NetworkSiteStatus_CREATED = NetworkSiteStatus' "CREATED"

pattern NetworkSiteStatus_DELETED :: NetworkSiteStatus
pattern NetworkSiteStatus_DELETED = NetworkSiteStatus' "DELETED"

pattern NetworkSiteStatus_DEPROVISIONING :: NetworkSiteStatus
pattern NetworkSiteStatus_DEPROVISIONING = NetworkSiteStatus' "DEPROVISIONING"

pattern NetworkSiteStatus_PROVISIONING :: NetworkSiteStatus
pattern NetworkSiteStatus_PROVISIONING = NetworkSiteStatus' "PROVISIONING"

{-# COMPLETE
  NetworkSiteStatus_AVAILABLE,
  NetworkSiteStatus_CREATED,
  NetworkSiteStatus_DELETED,
  NetworkSiteStatus_DEPROVISIONING,
  NetworkSiteStatus_PROVISIONING,
  NetworkSiteStatus'
  #-}
