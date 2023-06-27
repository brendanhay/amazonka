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
-- Module      : Amazonka.IoT.Types.OTAUpdateStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.OTAUpdateStatus
  ( OTAUpdateStatus
      ( ..,
        OTAUpdateStatus_CREATE_COMPLETE,
        OTAUpdateStatus_CREATE_FAILED,
        OTAUpdateStatus_CREATE_IN_PROGRESS,
        OTAUpdateStatus_CREATE_PENDING,
        OTAUpdateStatus_DELETE_FAILED,
        OTAUpdateStatus_DELETE_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OTAUpdateStatus = OTAUpdateStatus'
  { fromOTAUpdateStatus ::
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

pattern OTAUpdateStatus_CREATE_COMPLETE :: OTAUpdateStatus
pattern OTAUpdateStatus_CREATE_COMPLETE = OTAUpdateStatus' "CREATE_COMPLETE"

pattern OTAUpdateStatus_CREATE_FAILED :: OTAUpdateStatus
pattern OTAUpdateStatus_CREATE_FAILED = OTAUpdateStatus' "CREATE_FAILED"

pattern OTAUpdateStatus_CREATE_IN_PROGRESS :: OTAUpdateStatus
pattern OTAUpdateStatus_CREATE_IN_PROGRESS = OTAUpdateStatus' "CREATE_IN_PROGRESS"

pattern OTAUpdateStatus_CREATE_PENDING :: OTAUpdateStatus
pattern OTAUpdateStatus_CREATE_PENDING = OTAUpdateStatus' "CREATE_PENDING"

pattern OTAUpdateStatus_DELETE_FAILED :: OTAUpdateStatus
pattern OTAUpdateStatus_DELETE_FAILED = OTAUpdateStatus' "DELETE_FAILED"

pattern OTAUpdateStatus_DELETE_IN_PROGRESS :: OTAUpdateStatus
pattern OTAUpdateStatus_DELETE_IN_PROGRESS = OTAUpdateStatus' "DELETE_IN_PROGRESS"

{-# COMPLETE
  OTAUpdateStatus_CREATE_COMPLETE,
  OTAUpdateStatus_CREATE_FAILED,
  OTAUpdateStatus_CREATE_IN_PROGRESS,
  OTAUpdateStatus_CREATE_PENDING,
  OTAUpdateStatus_DELETE_FAILED,
  OTAUpdateStatus_DELETE_IN_PROGRESS,
  OTAUpdateStatus'
  #-}
