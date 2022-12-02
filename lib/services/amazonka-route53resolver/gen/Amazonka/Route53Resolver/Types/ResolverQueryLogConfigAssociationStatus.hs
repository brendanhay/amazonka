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
-- Module      : Amazonka.Route53Resolver.Types.ResolverQueryLogConfigAssociationStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Resolver.Types.ResolverQueryLogConfigAssociationStatus
  ( ResolverQueryLogConfigAssociationStatus
      ( ..,
        ResolverQueryLogConfigAssociationStatus_ACTION_NEEDED,
        ResolverQueryLogConfigAssociationStatus_ACTIVE,
        ResolverQueryLogConfigAssociationStatus_CREATING,
        ResolverQueryLogConfigAssociationStatus_DELETING,
        ResolverQueryLogConfigAssociationStatus_FAILED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResolverQueryLogConfigAssociationStatus = ResolverQueryLogConfigAssociationStatus'
  { fromResolverQueryLogConfigAssociationStatus ::
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

pattern ResolverQueryLogConfigAssociationStatus_ACTION_NEEDED :: ResolverQueryLogConfigAssociationStatus
pattern ResolverQueryLogConfigAssociationStatus_ACTION_NEEDED = ResolverQueryLogConfigAssociationStatus' "ACTION_NEEDED"

pattern ResolverQueryLogConfigAssociationStatus_ACTIVE :: ResolverQueryLogConfigAssociationStatus
pattern ResolverQueryLogConfigAssociationStatus_ACTIVE = ResolverQueryLogConfigAssociationStatus' "ACTIVE"

pattern ResolverQueryLogConfigAssociationStatus_CREATING :: ResolverQueryLogConfigAssociationStatus
pattern ResolverQueryLogConfigAssociationStatus_CREATING = ResolverQueryLogConfigAssociationStatus' "CREATING"

pattern ResolverQueryLogConfigAssociationStatus_DELETING :: ResolverQueryLogConfigAssociationStatus
pattern ResolverQueryLogConfigAssociationStatus_DELETING = ResolverQueryLogConfigAssociationStatus' "DELETING"

pattern ResolverQueryLogConfigAssociationStatus_FAILED :: ResolverQueryLogConfigAssociationStatus
pattern ResolverQueryLogConfigAssociationStatus_FAILED = ResolverQueryLogConfigAssociationStatus' "FAILED"

{-# COMPLETE
  ResolverQueryLogConfigAssociationStatus_ACTION_NEEDED,
  ResolverQueryLogConfigAssociationStatus_ACTIVE,
  ResolverQueryLogConfigAssociationStatus_CREATING,
  ResolverQueryLogConfigAssociationStatus_DELETING,
  ResolverQueryLogConfigAssociationStatus_FAILED,
  ResolverQueryLogConfigAssociationStatus'
  #-}
