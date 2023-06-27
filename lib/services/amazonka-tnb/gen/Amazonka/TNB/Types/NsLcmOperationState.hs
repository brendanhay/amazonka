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
-- Module      : Amazonka.TNB.Types.NsLcmOperationState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.NsLcmOperationState
  ( NsLcmOperationState
      ( ..,
        NsLcmOperationState_CANCELLED,
        NsLcmOperationState_CANCELLING,
        NsLcmOperationState_COMPLETED,
        NsLcmOperationState_FAILED,
        NsLcmOperationState_PROCESSING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NsLcmOperationState = NsLcmOperationState'
  { fromNsLcmOperationState ::
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

pattern NsLcmOperationState_CANCELLED :: NsLcmOperationState
pattern NsLcmOperationState_CANCELLED = NsLcmOperationState' "CANCELLED"

pattern NsLcmOperationState_CANCELLING :: NsLcmOperationState
pattern NsLcmOperationState_CANCELLING = NsLcmOperationState' "CANCELLING"

pattern NsLcmOperationState_COMPLETED :: NsLcmOperationState
pattern NsLcmOperationState_COMPLETED = NsLcmOperationState' "COMPLETED"

pattern NsLcmOperationState_FAILED :: NsLcmOperationState
pattern NsLcmOperationState_FAILED = NsLcmOperationState' "FAILED"

pattern NsLcmOperationState_PROCESSING :: NsLcmOperationState
pattern NsLcmOperationState_PROCESSING = NsLcmOperationState' "PROCESSING"

{-# COMPLETE
  NsLcmOperationState_CANCELLED,
  NsLcmOperationState_CANCELLING,
  NsLcmOperationState_COMPLETED,
  NsLcmOperationState_FAILED,
  NsLcmOperationState_PROCESSING,
  NsLcmOperationState'
  #-}
