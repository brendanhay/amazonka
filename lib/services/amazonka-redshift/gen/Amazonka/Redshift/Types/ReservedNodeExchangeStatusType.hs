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
-- Module      : Amazonka.Redshift.Types.ReservedNodeExchangeStatusType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.ReservedNodeExchangeStatusType
  ( ReservedNodeExchangeStatusType
      ( ..,
        ReservedNodeExchangeStatusType_FAILED,
        ReservedNodeExchangeStatusType_IN_PROGRESS,
        ReservedNodeExchangeStatusType_PENDING,
        ReservedNodeExchangeStatusType_REQUESTED,
        ReservedNodeExchangeStatusType_RETRYING,
        ReservedNodeExchangeStatusType_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

newtype ReservedNodeExchangeStatusType = ReservedNodeExchangeStatusType'
  { fromReservedNodeExchangeStatusType ::
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

pattern ReservedNodeExchangeStatusType_FAILED :: ReservedNodeExchangeStatusType
pattern ReservedNodeExchangeStatusType_FAILED = ReservedNodeExchangeStatusType' "FAILED"

pattern ReservedNodeExchangeStatusType_IN_PROGRESS :: ReservedNodeExchangeStatusType
pattern ReservedNodeExchangeStatusType_IN_PROGRESS = ReservedNodeExchangeStatusType' "IN_PROGRESS"

pattern ReservedNodeExchangeStatusType_PENDING :: ReservedNodeExchangeStatusType
pattern ReservedNodeExchangeStatusType_PENDING = ReservedNodeExchangeStatusType' "PENDING"

pattern ReservedNodeExchangeStatusType_REQUESTED :: ReservedNodeExchangeStatusType
pattern ReservedNodeExchangeStatusType_REQUESTED = ReservedNodeExchangeStatusType' "REQUESTED"

pattern ReservedNodeExchangeStatusType_RETRYING :: ReservedNodeExchangeStatusType
pattern ReservedNodeExchangeStatusType_RETRYING = ReservedNodeExchangeStatusType' "RETRYING"

pattern ReservedNodeExchangeStatusType_SUCCEEDED :: ReservedNodeExchangeStatusType
pattern ReservedNodeExchangeStatusType_SUCCEEDED = ReservedNodeExchangeStatusType' "SUCCEEDED"

{-# COMPLETE
  ReservedNodeExchangeStatusType_FAILED,
  ReservedNodeExchangeStatusType_IN_PROGRESS,
  ReservedNodeExchangeStatusType_PENDING,
  ReservedNodeExchangeStatusType_REQUESTED,
  ReservedNodeExchangeStatusType_RETRYING,
  ReservedNodeExchangeStatusType_SUCCEEDED,
  ReservedNodeExchangeStatusType'
  #-}
