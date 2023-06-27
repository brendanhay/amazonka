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
-- Module      : Amazonka.EC2.Types.IpamDiscoveryFailureCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.IpamDiscoveryFailureCode
  ( IpamDiscoveryFailureCode
      ( ..,
        IpamDiscoveryFailureCode_Assume_role_failure,
        IpamDiscoveryFailureCode_Throttling_failure,
        IpamDiscoveryFailureCode_Unauthorized_failure
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype IpamDiscoveryFailureCode = IpamDiscoveryFailureCode'
  { fromIpamDiscoveryFailureCode ::
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

pattern IpamDiscoveryFailureCode_Assume_role_failure :: IpamDiscoveryFailureCode
pattern IpamDiscoveryFailureCode_Assume_role_failure = IpamDiscoveryFailureCode' "assume-role-failure"

pattern IpamDiscoveryFailureCode_Throttling_failure :: IpamDiscoveryFailureCode
pattern IpamDiscoveryFailureCode_Throttling_failure = IpamDiscoveryFailureCode' "throttling-failure"

pattern IpamDiscoveryFailureCode_Unauthorized_failure :: IpamDiscoveryFailureCode
pattern IpamDiscoveryFailureCode_Unauthorized_failure = IpamDiscoveryFailureCode' "unauthorized-failure"

{-# COMPLETE
  IpamDiscoveryFailureCode_Assume_role_failure,
  IpamDiscoveryFailureCode_Throttling_failure,
  IpamDiscoveryFailureCode_Unauthorized_failure,
  IpamDiscoveryFailureCode'
  #-}
