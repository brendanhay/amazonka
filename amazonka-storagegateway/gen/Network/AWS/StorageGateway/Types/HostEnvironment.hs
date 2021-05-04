{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype HostEnvironment = HostEnvironment'
  { fromHostEnvironment ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
