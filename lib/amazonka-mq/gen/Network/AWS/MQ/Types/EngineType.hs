{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.EngineType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.EngineType
  ( EngineType
      ( EngineType',
        Activemq,
        Rabbitmq
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The type of broker engine. Note: Currently, Amazon MQ supports ActiveMQ and RabbitMQ.
newtype EngineType = EngineType' Lude.Text
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

pattern Activemq :: EngineType
pattern Activemq = EngineType' "ACTIVEMQ"

pattern Rabbitmq :: EngineType
pattern Rabbitmq = EngineType' "RABBITMQ"

{-# COMPLETE
  Activemq,
  Rabbitmq,
  EngineType'
  #-}
