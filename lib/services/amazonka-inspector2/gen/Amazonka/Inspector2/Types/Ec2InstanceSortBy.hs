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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
import qualified Amazonka.Prelude as Prelude

newtype Ec2InstanceSortBy = Ec2InstanceSortBy'
  { fromEc2InstanceSortBy ::
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
