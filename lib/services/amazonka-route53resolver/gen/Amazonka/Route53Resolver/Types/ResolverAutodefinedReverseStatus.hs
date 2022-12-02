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
-- Module      : Amazonka.Route53Resolver.Types.ResolverAutodefinedReverseStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Resolver.Types.ResolverAutodefinedReverseStatus
  ( ResolverAutodefinedReverseStatus
      ( ..,
        ResolverAutodefinedReverseStatus_DISABLED,
        ResolverAutodefinedReverseStatus_DISABLING,
        ResolverAutodefinedReverseStatus_ENABLED,
        ResolverAutodefinedReverseStatus_ENABLING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResolverAutodefinedReverseStatus = ResolverAutodefinedReverseStatus'
  { fromResolverAutodefinedReverseStatus ::
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

pattern ResolverAutodefinedReverseStatus_DISABLED :: ResolverAutodefinedReverseStatus
pattern ResolverAutodefinedReverseStatus_DISABLED = ResolverAutodefinedReverseStatus' "DISABLED"

pattern ResolverAutodefinedReverseStatus_DISABLING :: ResolverAutodefinedReverseStatus
pattern ResolverAutodefinedReverseStatus_DISABLING = ResolverAutodefinedReverseStatus' "DISABLING"

pattern ResolverAutodefinedReverseStatus_ENABLED :: ResolverAutodefinedReverseStatus
pattern ResolverAutodefinedReverseStatus_ENABLED = ResolverAutodefinedReverseStatus' "ENABLED"

pattern ResolverAutodefinedReverseStatus_ENABLING :: ResolverAutodefinedReverseStatus
pattern ResolverAutodefinedReverseStatus_ENABLING = ResolverAutodefinedReverseStatus' "ENABLING"

{-# COMPLETE
  ResolverAutodefinedReverseStatus_DISABLED,
  ResolverAutodefinedReverseStatus_DISABLING,
  ResolverAutodefinedReverseStatus_ENABLED,
  ResolverAutodefinedReverseStatus_ENABLING,
  ResolverAutodefinedReverseStatus'
  #-}
