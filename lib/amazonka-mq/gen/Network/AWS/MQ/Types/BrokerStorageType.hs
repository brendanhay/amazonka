{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.BrokerStorageType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.BrokerStorageType
  ( BrokerStorageType
      ( BrokerStorageType',
        EBS,
        Efs
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | /Important:/ EFS is currently not Supported for RabbitMQ engine type.
newtype BrokerStorageType = BrokerStorageType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern EBS :: BrokerStorageType
pattern EBS = BrokerStorageType' "EBS"

pattern Efs :: BrokerStorageType
pattern Efs = BrokerStorageType' "EFS"

{-# COMPLETE
  EBS,
  Efs,
  BrokerStorageType'
  #-}
