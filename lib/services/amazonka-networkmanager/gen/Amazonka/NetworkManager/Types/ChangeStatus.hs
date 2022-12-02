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
-- Module      : Amazonka.NetworkManager.Types.ChangeStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.ChangeStatus
  ( ChangeStatus
      ( ..,
        ChangeStatus_COMPLETE,
        ChangeStatus_FAILED,
        ChangeStatus_IN_PROGRESS,
        ChangeStatus_NOT_STARTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ChangeStatus = ChangeStatus'
  { fromChangeStatus ::
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

pattern ChangeStatus_COMPLETE :: ChangeStatus
pattern ChangeStatus_COMPLETE = ChangeStatus' "COMPLETE"

pattern ChangeStatus_FAILED :: ChangeStatus
pattern ChangeStatus_FAILED = ChangeStatus' "FAILED"

pattern ChangeStatus_IN_PROGRESS :: ChangeStatus
pattern ChangeStatus_IN_PROGRESS = ChangeStatus' "IN_PROGRESS"

pattern ChangeStatus_NOT_STARTED :: ChangeStatus
pattern ChangeStatus_NOT_STARTED = ChangeStatus' "NOT_STARTED"

{-# COMPLETE
  ChangeStatus_COMPLETE,
  ChangeStatus_FAILED,
  ChangeStatus_IN_PROGRESS,
  ChangeStatus_NOT_STARTED,
  ChangeStatus'
  #-}
