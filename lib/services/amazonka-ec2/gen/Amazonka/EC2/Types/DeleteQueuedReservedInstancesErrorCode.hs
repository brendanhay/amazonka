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
-- Module      : Amazonka.EC2.Types.DeleteQueuedReservedInstancesErrorCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.DeleteQueuedReservedInstancesErrorCode
  ( DeleteQueuedReservedInstancesErrorCode
      ( ..,
        DeleteQueuedReservedInstancesErrorCode_Reserved_instances_id_invalid,
        DeleteQueuedReservedInstancesErrorCode_Reserved_instances_not_in_queued_state,
        DeleteQueuedReservedInstancesErrorCode_Unexpected_error
      ),
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype DeleteQueuedReservedInstancesErrorCode = DeleteQueuedReservedInstancesErrorCode'
  { fromDeleteQueuedReservedInstancesErrorCode ::
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

pattern DeleteQueuedReservedInstancesErrorCode_Reserved_instances_id_invalid :: DeleteQueuedReservedInstancesErrorCode
pattern DeleteQueuedReservedInstancesErrorCode_Reserved_instances_id_invalid = DeleteQueuedReservedInstancesErrorCode' "reserved-instances-id-invalid"

pattern DeleteQueuedReservedInstancesErrorCode_Reserved_instances_not_in_queued_state :: DeleteQueuedReservedInstancesErrorCode
pattern DeleteQueuedReservedInstancesErrorCode_Reserved_instances_not_in_queued_state = DeleteQueuedReservedInstancesErrorCode' "reserved-instances-not-in-queued-state"

pattern DeleteQueuedReservedInstancesErrorCode_Unexpected_error :: DeleteQueuedReservedInstancesErrorCode
pattern DeleteQueuedReservedInstancesErrorCode_Unexpected_error = DeleteQueuedReservedInstancesErrorCode' "unexpected-error"

{-# COMPLETE
  DeleteQueuedReservedInstancesErrorCode_Reserved_instances_id_invalid,
  DeleteQueuedReservedInstancesErrorCode_Reserved_instances_not_in_queued_state,
  DeleteQueuedReservedInstancesErrorCode_Unexpected_error,
  DeleteQueuedReservedInstancesErrorCode'
  #-}
