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
-- Module      : Network.AWS.Kinesis.Types.ConsumerStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.ConsumerStatus
  ( ConsumerStatus
      ( ..,
        ConsumerStatus_ACTIVE,
        ConsumerStatus_CREATING,
        ConsumerStatus_DELETING
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ConsumerStatus = ConsumerStatus'
  { fromConsumerStatus ::
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

pattern ConsumerStatus_ACTIVE :: ConsumerStatus
pattern ConsumerStatus_ACTIVE = ConsumerStatus' "ACTIVE"

pattern ConsumerStatus_CREATING :: ConsumerStatus
pattern ConsumerStatus_CREATING = ConsumerStatus' "CREATING"

pattern ConsumerStatus_DELETING :: ConsumerStatus
pattern ConsumerStatus_DELETING = ConsumerStatus' "DELETING"

{-# COMPLETE
  ConsumerStatus_ACTIVE,
  ConsumerStatus_CREATING,
  ConsumerStatus_DELETING,
  ConsumerStatus'
  #-}
