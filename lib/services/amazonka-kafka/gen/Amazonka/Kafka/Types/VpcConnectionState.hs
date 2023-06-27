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
-- Module      : Amazonka.Kafka.Types.VpcConnectionState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.VpcConnectionState
  ( VpcConnectionState
      ( ..,
        VpcConnectionState_AVAILABLE,
        VpcConnectionState_CREATING,
        VpcConnectionState_DEACTIVATING,
        VpcConnectionState_DELETING,
        VpcConnectionState_FAILED,
        VpcConnectionState_INACTIVE,
        VpcConnectionState_REJECTED,
        VpcConnectionState_REJECTING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The state of a VPC connection.
newtype VpcConnectionState = VpcConnectionState'
  { fromVpcConnectionState ::
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

pattern VpcConnectionState_AVAILABLE :: VpcConnectionState
pattern VpcConnectionState_AVAILABLE = VpcConnectionState' "AVAILABLE"

pattern VpcConnectionState_CREATING :: VpcConnectionState
pattern VpcConnectionState_CREATING = VpcConnectionState' "CREATING"

pattern VpcConnectionState_DEACTIVATING :: VpcConnectionState
pattern VpcConnectionState_DEACTIVATING = VpcConnectionState' "DEACTIVATING"

pattern VpcConnectionState_DELETING :: VpcConnectionState
pattern VpcConnectionState_DELETING = VpcConnectionState' "DELETING"

pattern VpcConnectionState_FAILED :: VpcConnectionState
pattern VpcConnectionState_FAILED = VpcConnectionState' "FAILED"

pattern VpcConnectionState_INACTIVE :: VpcConnectionState
pattern VpcConnectionState_INACTIVE = VpcConnectionState' "INACTIVE"

pattern VpcConnectionState_REJECTED :: VpcConnectionState
pattern VpcConnectionState_REJECTED = VpcConnectionState' "REJECTED"

pattern VpcConnectionState_REJECTING :: VpcConnectionState
pattern VpcConnectionState_REJECTING = VpcConnectionState' "REJECTING"

{-# COMPLETE
  VpcConnectionState_AVAILABLE,
  VpcConnectionState_CREATING,
  VpcConnectionState_DEACTIVATING,
  VpcConnectionState_DELETING,
  VpcConnectionState_FAILED,
  VpcConnectionState_INACTIVE,
  VpcConnectionState_REJECTED,
  VpcConnectionState_REJECTING,
  VpcConnectionState'
  #-}
