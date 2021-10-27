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
-- Module      : Network.AWS.Chime.Types.CallingNameStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Chime.Types.CallingNameStatus
  ( CallingNameStatus
      ( ..,
        CallingNameStatus_Unassigned,
        CallingNameStatus_UpdateFailed,
        CallingNameStatus_UpdateInProgress,
        CallingNameStatus_UpdateSucceeded
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype CallingNameStatus = CallingNameStatus'
  { fromCallingNameStatus ::
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
