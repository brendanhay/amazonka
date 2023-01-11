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
-- Module      : Amazonka.ELBV2.Types.TargetGroupIpAddressTypeEnum
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types.TargetGroupIpAddressTypeEnum
  ( TargetGroupIpAddressTypeEnum
      ( ..,
        TargetGroupIpAddressTypeEnum_Ipv4,
        TargetGroupIpAddressTypeEnum_Ipv6
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TargetGroupIpAddressTypeEnum = TargetGroupIpAddressTypeEnum'
  { fromTargetGroupIpAddressTypeEnum ::
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

pattern TargetGroupIpAddressTypeEnum_Ipv4 :: TargetGroupIpAddressTypeEnum
pattern TargetGroupIpAddressTypeEnum_Ipv4 = TargetGroupIpAddressTypeEnum' "ipv4"

pattern TargetGroupIpAddressTypeEnum_Ipv6 :: TargetGroupIpAddressTypeEnum
pattern TargetGroupIpAddressTypeEnum_Ipv6 = TargetGroupIpAddressTypeEnum' "ipv6"

{-# COMPLETE
  TargetGroupIpAddressTypeEnum_Ipv4,
  TargetGroupIpAddressTypeEnum_Ipv6,
  TargetGroupIpAddressTypeEnum'
  #-}
