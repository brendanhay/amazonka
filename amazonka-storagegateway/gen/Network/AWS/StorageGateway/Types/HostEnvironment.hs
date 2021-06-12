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
-- Module      : Network.AWS.StorageGateway.Types.HostEnvironment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.HostEnvironment
  ( HostEnvironment
      ( ..,
        HostEnvironment_EC2,
        HostEnvironment_HYPER_V,
        HostEnvironment_KVM,
        HostEnvironment_OTHER,
        HostEnvironment_VMWARE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype HostEnvironment = HostEnvironment'
  { fromHostEnvironment ::
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

pattern HostEnvironment_EC2 :: HostEnvironment
pattern HostEnvironment_EC2 = HostEnvironment' "EC2"

pattern HostEnvironment_HYPER_V :: HostEnvironment
pattern HostEnvironment_HYPER_V = HostEnvironment' "HYPER-V"

pattern HostEnvironment_KVM :: HostEnvironment
pattern HostEnvironment_KVM = HostEnvironment' "KVM"

pattern HostEnvironment_OTHER :: HostEnvironment
pattern HostEnvironment_OTHER = HostEnvironment' "OTHER"

pattern HostEnvironment_VMWARE :: HostEnvironment
pattern HostEnvironment_VMWARE = HostEnvironment' "VMWARE"

{-# COMPLETE
  HostEnvironment_EC2,
  HostEnvironment_HYPER_V,
  HostEnvironment_KVM,
  HostEnvironment_OTHER,
  HostEnvironment_VMWARE,
  HostEnvironment'
  #-}
