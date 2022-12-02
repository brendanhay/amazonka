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
-- Module      : Amazonka.Route53Resolver.Types.FirewallDomainUpdateOperation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Resolver.Types.FirewallDomainUpdateOperation
  ( FirewallDomainUpdateOperation
      ( ..,
        FirewallDomainUpdateOperation_ADD,
        FirewallDomainUpdateOperation_REMOVE,
        FirewallDomainUpdateOperation_REPLACE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FirewallDomainUpdateOperation = FirewallDomainUpdateOperation'
  { fromFirewallDomainUpdateOperation ::
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

pattern FirewallDomainUpdateOperation_ADD :: FirewallDomainUpdateOperation
pattern FirewallDomainUpdateOperation_ADD = FirewallDomainUpdateOperation' "ADD"

pattern FirewallDomainUpdateOperation_REMOVE :: FirewallDomainUpdateOperation
pattern FirewallDomainUpdateOperation_REMOVE = FirewallDomainUpdateOperation' "REMOVE"

pattern FirewallDomainUpdateOperation_REPLACE :: FirewallDomainUpdateOperation
pattern FirewallDomainUpdateOperation_REPLACE = FirewallDomainUpdateOperation' "REPLACE"

{-# COMPLETE
  FirewallDomainUpdateOperation_ADD,
  FirewallDomainUpdateOperation_REMOVE,
  FirewallDomainUpdateOperation_REPLACE,
  FirewallDomainUpdateOperation'
  #-}
