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
-- Module      : Amazonka.Kafka.Types.BrokerAZDistribution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.BrokerAZDistribution
  ( BrokerAZDistribution
      ( ..,
        BrokerAZDistribution_DEFAULT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The distribution of broker nodes across Availability Zones. This is an
-- optional parameter. If you don\'t specify it, Amazon MSK gives it the
-- value DEFAULT. You can also explicitly set this parameter to the value
-- DEFAULT. No other values are currently allowed.
--
-- Amazon MSK distributes the broker nodes evenly across the Availability
-- Zones that correspond to the subnets you provide when you create the
-- cluster.
newtype BrokerAZDistribution = BrokerAZDistribution'
  { fromBrokerAZDistribution ::
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

pattern BrokerAZDistribution_DEFAULT :: BrokerAZDistribution
pattern BrokerAZDistribution_DEFAULT = BrokerAZDistribution' "DEFAULT"

{-# COMPLETE
  BrokerAZDistribution_DEFAULT,
  BrokerAZDistribution'
  #-}
