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
-- Module      : Amazonka.Chime.Types.CallingNameStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.CallingNameStatus
  ( CallingNameStatus
      ( ..,
        CallingNameStatus_Unassigned,
        CallingNameStatus_UpdateFailed,
        CallingNameStatus_UpdateInProgress,
        CallingNameStatus_UpdateSucceeded
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CallingNameStatus = CallingNameStatus'
  { fromCallingNameStatus ::
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

pattern CallingNameStatus_Unassigned :: CallingNameStatus
pattern CallingNameStatus_Unassigned = CallingNameStatus' "Unassigned"

pattern CallingNameStatus_UpdateFailed :: CallingNameStatus
pattern CallingNameStatus_UpdateFailed = CallingNameStatus' "UpdateFailed"

pattern CallingNameStatus_UpdateInProgress :: CallingNameStatus
pattern CallingNameStatus_UpdateInProgress = CallingNameStatus' "UpdateInProgress"

pattern CallingNameStatus_UpdateSucceeded :: CallingNameStatus
pattern CallingNameStatus_UpdateSucceeded = CallingNameStatus' "UpdateSucceeded"

{-# COMPLETE
  CallingNameStatus_Unassigned,
  CallingNameStatus_UpdateFailed,
  CallingNameStatus_UpdateInProgress,
  CallingNameStatus_UpdateSucceeded,
  CallingNameStatus'
  #-}
