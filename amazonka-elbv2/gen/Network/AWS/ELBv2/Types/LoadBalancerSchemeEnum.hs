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
-- Module      : Network.AWS.ELBv2.Types.LoadBalancerSchemeEnum
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.LoadBalancerSchemeEnum
  ( LoadBalancerSchemeEnum
      ( ..,
        LoadBalancerSchemeEnum_Internal,
        LoadBalancerSchemeEnum_Internet_facing
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype LoadBalancerSchemeEnum = LoadBalancerSchemeEnum'
  { fromLoadBalancerSchemeEnum ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern LoadBalancerSchemeEnum_Internal :: LoadBalancerSchemeEnum
pattern LoadBalancerSchemeEnum_Internal = LoadBalancerSchemeEnum' "internal"

pattern LoadBalancerSchemeEnum_Internet_facing :: LoadBalancerSchemeEnum
pattern LoadBalancerSchemeEnum_Internet_facing = LoadBalancerSchemeEnum' "internet-facing"

{-# COMPLETE
  LoadBalancerSchemeEnum_Internal,
  LoadBalancerSchemeEnum_Internet_facing,
  LoadBalancerSchemeEnum'
  #-}
