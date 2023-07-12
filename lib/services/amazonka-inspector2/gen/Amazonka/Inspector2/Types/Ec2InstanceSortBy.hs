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
-- Module      : Amazonka.Inspector2.Types.Ec2InstanceSortBy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.Ec2InstanceSortBy
  ( Ec2InstanceSortBy
      ( ..,
        Ec2InstanceSortBy_ALL,
        Ec2InstanceSortBy_CRITICAL,
        Ec2InstanceSortBy_HIGH,
        Ec2InstanceSortBy_NETWORK_FINDINGS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Ec2InstanceSortBy = Ec2InstanceSortBy'
  { fromEc2InstanceSortBy ::
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

pattern Ec2InstanceSortBy_ALL :: Ec2InstanceSortBy
pattern Ec2InstanceSortBy_ALL = Ec2InstanceSortBy' "ALL"

pattern Ec2InstanceSortBy_CRITICAL :: Ec2InstanceSortBy
pattern Ec2InstanceSortBy_CRITICAL = Ec2InstanceSortBy' "CRITICAL"

pattern Ec2InstanceSortBy_HIGH :: Ec2InstanceSortBy
pattern Ec2InstanceSortBy_HIGH = Ec2InstanceSortBy' "HIGH"

pattern Ec2InstanceSortBy_NETWORK_FINDINGS :: Ec2InstanceSortBy
pattern Ec2InstanceSortBy_NETWORK_FINDINGS = Ec2InstanceSortBy' "NETWORK_FINDINGS"

{-# COMPLETE
  Ec2InstanceSortBy_ALL,
  Ec2InstanceSortBy_CRITICAL,
  Ec2InstanceSortBy_HIGH,
  Ec2InstanceSortBy_NETWORK_FINDINGS,
  Ec2InstanceSortBy'
  #-}
