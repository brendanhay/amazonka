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

import qualified Network.AWS.Prelude as Prelude

-- | The broker\'s storage type.
-- EFS is currently not Supported for RabbitMQ engine type.
newtype BrokerStorageType = BrokerStorageType'
  { fromBrokerStorageType ::
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

pattern BrokerStorageType_EBS :: BrokerStorageType
pattern BrokerStorageType_EBS = BrokerStorageType' "EBS"

pattern BrokerStorageType_EFS :: BrokerStorageType
pattern BrokerStorageType_EFS = BrokerStorageType' "EFS"

{-# COMPLETE
  BrokerStorageType_EBS,
  BrokerStorageType_EFS,
  BrokerStorageType'
  #-}
