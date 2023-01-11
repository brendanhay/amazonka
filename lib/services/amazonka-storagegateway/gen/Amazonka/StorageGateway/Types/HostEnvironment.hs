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
-- Module      : Amazonka.StorageGateway.Types.HostEnvironment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.HostEnvironment
  ( HostEnvironment
      ( ..,
        HostEnvironment_EC2,
        HostEnvironment_HYPER_V,
        HostEnvironment_KVM,
        HostEnvironment_OTHER,
        HostEnvironment_SNOWBALL,
        HostEnvironment_VMWARE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype HostEnvironment = HostEnvironment'
  { fromHostEnvironment ::
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

pattern HostEnvironment_EC2 :: HostEnvironment
pattern HostEnvironment_EC2 = HostEnvironment' "EC2"

pattern HostEnvironment_HYPER_V :: HostEnvironment
pattern HostEnvironment_HYPER_V = HostEnvironment' "HYPER-V"

pattern HostEnvironment_KVM :: HostEnvironment
pattern HostEnvironment_KVM = HostEnvironment' "KVM"

pattern HostEnvironment_OTHER :: HostEnvironment
pattern HostEnvironment_OTHER = HostEnvironment' "OTHER"

pattern HostEnvironment_SNOWBALL :: HostEnvironment
pattern HostEnvironment_SNOWBALL = HostEnvironment' "SNOWBALL"

pattern HostEnvironment_VMWARE :: HostEnvironment
pattern HostEnvironment_VMWARE = HostEnvironment' "VMWARE"

{-# COMPLETE
  HostEnvironment_EC2,
  HostEnvironment_HYPER_V,
  HostEnvironment_KVM,
  HostEnvironment_OTHER,
  HostEnvironment_SNOWBALL,
  HostEnvironment_VMWARE,
  HostEnvironment'
  #-}
