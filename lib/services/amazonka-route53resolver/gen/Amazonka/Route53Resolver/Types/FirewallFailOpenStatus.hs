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
-- Module      : Amazonka.Route53Resolver.Types.FirewallFailOpenStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Resolver.Types.FirewallFailOpenStatus
  ( FirewallFailOpenStatus
      ( ..,
        FirewallFailOpenStatus_DISABLED,
        FirewallFailOpenStatus_ENABLED,
        FirewallFailOpenStatus_USE_LOCAL_RESOURCE_SETTING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FirewallFailOpenStatus = FirewallFailOpenStatus'
  { fromFirewallFailOpenStatus ::
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

pattern FirewallFailOpenStatus_DISABLED :: FirewallFailOpenStatus
pattern FirewallFailOpenStatus_DISABLED = FirewallFailOpenStatus' "DISABLED"

pattern FirewallFailOpenStatus_ENABLED :: FirewallFailOpenStatus
pattern FirewallFailOpenStatus_ENABLED = FirewallFailOpenStatus' "ENABLED"

pattern FirewallFailOpenStatus_USE_LOCAL_RESOURCE_SETTING :: FirewallFailOpenStatus
pattern FirewallFailOpenStatus_USE_LOCAL_RESOURCE_SETTING = FirewallFailOpenStatus' "USE_LOCAL_RESOURCE_SETTING"

{-# COMPLETE
  FirewallFailOpenStatus_DISABLED,
  FirewallFailOpenStatus_ENABLED,
  FirewallFailOpenStatus_USE_LOCAL_RESOURCE_SETTING,
  FirewallFailOpenStatus'
  #-}
