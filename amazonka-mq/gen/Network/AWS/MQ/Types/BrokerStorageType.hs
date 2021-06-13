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
-- Module      : Network.AWS.MQ.Types.BrokerStorageType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.BrokerStorageType
  ( BrokerStorageType
      ( ..,
        BrokerStorageType_EBS,
        BrokerStorageType_EFS
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | The broker\'s storage type.
-- EFS is currently not Supported for RabbitMQ engine type.
newtype BrokerStorageType = BrokerStorageType'
  { fromBrokerStorageType ::
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

pattern BrokerStorageType_EBS :: BrokerStorageType
pattern BrokerStorageType_EBS = BrokerStorageType' "EBS"

pattern BrokerStorageType_EFS :: BrokerStorageType
pattern BrokerStorageType_EFS = BrokerStorageType' "EFS"

{-# COMPLETE
  BrokerStorageType_EBS,
  BrokerStorageType_EFS,
  BrokerStorageType'
  #-}
