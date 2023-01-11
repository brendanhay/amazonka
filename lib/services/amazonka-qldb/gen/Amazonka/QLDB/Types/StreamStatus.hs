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
-- Module      : Amazonka.QLDB.Types.StreamStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QLDB.Types.StreamStatus
  ( StreamStatus
      ( ..,
        StreamStatus_ACTIVE,
        StreamStatus_CANCELED,
        StreamStatus_COMPLETED,
        StreamStatus_FAILED,
        StreamStatus_IMPAIRED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StreamStatus = StreamStatus'
  { fromStreamStatus ::
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

pattern StreamStatus_ACTIVE :: StreamStatus
pattern StreamStatus_ACTIVE = StreamStatus' "ACTIVE"

pattern StreamStatus_CANCELED :: StreamStatus
pattern StreamStatus_CANCELED = StreamStatus' "CANCELED"

pattern StreamStatus_COMPLETED :: StreamStatus
pattern StreamStatus_COMPLETED = StreamStatus' "COMPLETED"

pattern StreamStatus_FAILED :: StreamStatus
pattern StreamStatus_FAILED = StreamStatus' "FAILED"

pattern StreamStatus_IMPAIRED :: StreamStatus
pattern StreamStatus_IMPAIRED = StreamStatus' "IMPAIRED"

{-# COMPLETE
  StreamStatus_ACTIVE,
  StreamStatus_CANCELED,
  StreamStatus_COMPLETED,
  StreamStatus_FAILED,
  StreamStatus_IMPAIRED,
  StreamStatus'
  #-}
