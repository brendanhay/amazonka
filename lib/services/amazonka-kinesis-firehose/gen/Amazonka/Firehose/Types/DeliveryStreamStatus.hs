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
-- Module      : Amazonka.Firehose.Types.DeliveryStreamStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.DeliveryStreamStatus
  ( DeliveryStreamStatus
      ( ..,
        DeliveryStreamStatus_ACTIVE,
        DeliveryStreamStatus_CREATING,
        DeliveryStreamStatus_CREATING_FAILED,
        DeliveryStreamStatus_DELETING,
        DeliveryStreamStatus_DELETING_FAILED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype DeliveryStreamStatus = DeliveryStreamStatus'
  { fromDeliveryStreamStatus ::
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

pattern DeliveryStreamStatus_ACTIVE :: DeliveryStreamStatus
pattern DeliveryStreamStatus_ACTIVE = DeliveryStreamStatus' "ACTIVE"

pattern DeliveryStreamStatus_CREATING :: DeliveryStreamStatus
pattern DeliveryStreamStatus_CREATING = DeliveryStreamStatus' "CREATING"

pattern DeliveryStreamStatus_CREATING_FAILED :: DeliveryStreamStatus
pattern DeliveryStreamStatus_CREATING_FAILED = DeliveryStreamStatus' "CREATING_FAILED"

pattern DeliveryStreamStatus_DELETING :: DeliveryStreamStatus
pattern DeliveryStreamStatus_DELETING = DeliveryStreamStatus' "DELETING"

pattern DeliveryStreamStatus_DELETING_FAILED :: DeliveryStreamStatus
pattern DeliveryStreamStatus_DELETING_FAILED = DeliveryStreamStatus' "DELETING_FAILED"

{-# COMPLETE
  DeliveryStreamStatus_ACTIVE,
  DeliveryStreamStatus_CREATING,
  DeliveryStreamStatus_CREATING_FAILED,
  DeliveryStreamStatus_DELETING,
  DeliveryStreamStatus_DELETING_FAILED,
  DeliveryStreamStatus'
  #-}
