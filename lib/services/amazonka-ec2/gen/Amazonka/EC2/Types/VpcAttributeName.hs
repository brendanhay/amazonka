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
-- Module      : Amazonka.EC2.Types.VpcAttributeName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VpcAttributeName
  ( VpcAttributeName
      ( ..,
        VpcAttributeName_EnableDnsHostnames,
        VpcAttributeName_EnableDnsSupport,
        VpcAttributeName_EnableNetworkAddressUsageMetrics
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype VpcAttributeName = VpcAttributeName'
  { fromVpcAttributeName ::
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

pattern VpcAttributeName_EnableDnsHostnames :: VpcAttributeName
pattern VpcAttributeName_EnableDnsHostnames = VpcAttributeName' "enableDnsHostnames"

pattern VpcAttributeName_EnableDnsSupport :: VpcAttributeName
pattern VpcAttributeName_EnableDnsSupport = VpcAttributeName' "enableDnsSupport"

pattern VpcAttributeName_EnableNetworkAddressUsageMetrics :: VpcAttributeName
pattern VpcAttributeName_EnableNetworkAddressUsageMetrics = VpcAttributeName' "enableNetworkAddressUsageMetrics"

{-# COMPLETE
  VpcAttributeName_EnableDnsHostnames,
  VpcAttributeName_EnableDnsSupport,
  VpcAttributeName_EnableNetworkAddressUsageMetrics,
  VpcAttributeName'
  #-}
