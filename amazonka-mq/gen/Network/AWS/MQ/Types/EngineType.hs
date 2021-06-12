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
-- Module      : Network.AWS.MQ.Types.EngineType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.EngineType
  ( EngineType
      ( ..,
        EngineType_ACTIVEMQ,
        EngineType_RABBITMQ
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | The type of broker engine. Note: Currently, Amazon MQ supports ActiveMQ
-- and RabbitMQ.
newtype EngineType = EngineType'
  { fromEngineType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern EngineType_ACTIVEMQ :: EngineType
pattern EngineType_ACTIVEMQ = EngineType' "ACTIVEMQ"

pattern EngineType_RABBITMQ :: EngineType
pattern EngineType_RABBITMQ = EngineType' "RABBITMQ"

{-# COMPLETE
  EngineType_ACTIVEMQ,
  EngineType_RABBITMQ,
  EngineType'
  #-}
