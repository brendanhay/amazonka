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
-- Module      : Network.AWS.ELBV2.Types.TargetGroupIpAddressTypeEnum
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBV2.Types.TargetGroupIpAddressTypeEnum
  ( TargetGroupIpAddressTypeEnum
      ( ..,
        TargetGroupIpAddressTypeEnum_Ipv4,
        TargetGroupIpAddressTypeEnum_Ipv6
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype TargetGroupIpAddressTypeEnum = TargetGroupIpAddressTypeEnum'
  { fromTargetGroupIpAddressTypeEnum ::
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

pattern TargetGroupIpAddressTypeEnum_Ipv4 :: TargetGroupIpAddressTypeEnum
pattern TargetGroupIpAddressTypeEnum_Ipv4 = TargetGroupIpAddressTypeEnum' "ipv4"

pattern TargetGroupIpAddressTypeEnum_Ipv6 :: TargetGroupIpAddressTypeEnum
pattern TargetGroupIpAddressTypeEnum_Ipv6 = TargetGroupIpAddressTypeEnum' "ipv6"

{-# COMPLETE
  TargetGroupIpAddressTypeEnum_Ipv4,
  TargetGroupIpAddressTypeEnum_Ipv6,
  TargetGroupIpAddressTypeEnum'
  #-}
