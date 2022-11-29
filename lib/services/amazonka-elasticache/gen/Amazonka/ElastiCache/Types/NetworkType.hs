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
-- Module      : Amazonka.ElastiCache.Types.NetworkType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.NetworkType
  ( NetworkType
      ( ..,
        NetworkType_Dual_stack,
        NetworkType_Ipv4,
        NetworkType_Ipv6
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype NetworkType = NetworkType'
  { fromNetworkType ::
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

pattern NetworkType_Dual_stack :: NetworkType
pattern NetworkType_Dual_stack = NetworkType' "dual_stack"

pattern NetworkType_Ipv4 :: NetworkType
pattern NetworkType_Ipv4 = NetworkType' "ipv4"

pattern NetworkType_Ipv6 :: NetworkType
pattern NetworkType_Ipv6 = NetworkType' "ipv6"

{-# COMPLETE
  NetworkType_Dual_stack,
  NetworkType_Ipv4,
  NetworkType_Ipv6,
  NetworkType'
  #-}
