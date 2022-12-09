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
-- Module      : Amazonka.Omics.Types.ReadSetActivationJobItemStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ReadSetActivationJobItemStatus
  ( ReadSetActivationJobItemStatus
      ( ..,
        ReadSetActivationJobItemStatus_FAILED,
        ReadSetActivationJobItemStatus_FINISHED,
        ReadSetActivationJobItemStatus_IN_PROGRESS,
        ReadSetActivationJobItemStatus_NOT_STARTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ReadSetActivationJobItemStatus = ReadSetActivationJobItemStatus'
  { fromReadSetActivationJobItemStatus ::
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

pattern ReadSetActivationJobItemStatus_FAILED :: ReadSetActivationJobItemStatus
pattern ReadSetActivationJobItemStatus_FAILED = ReadSetActivationJobItemStatus' "FAILED"

pattern ReadSetActivationJobItemStatus_FINISHED :: ReadSetActivationJobItemStatus
pattern ReadSetActivationJobItemStatus_FINISHED = ReadSetActivationJobItemStatus' "FINISHED"

pattern ReadSetActivationJobItemStatus_IN_PROGRESS :: ReadSetActivationJobItemStatus
pattern ReadSetActivationJobItemStatus_IN_PROGRESS = ReadSetActivationJobItemStatus' "IN_PROGRESS"

pattern ReadSetActivationJobItemStatus_NOT_STARTED :: ReadSetActivationJobItemStatus
pattern ReadSetActivationJobItemStatus_NOT_STARTED = ReadSetActivationJobItemStatus' "NOT_STARTED"

{-# COMPLETE
  ReadSetActivationJobItemStatus_FAILED,
  ReadSetActivationJobItemStatus_FINISHED,
  ReadSetActivationJobItemStatus_IN_PROGRESS,
  ReadSetActivationJobItemStatus_NOT_STARTED,
  ReadSetActivationJobItemStatus'
  #-}
