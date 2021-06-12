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
-- Module      : Network.AWS.SSM.Types.CommandFilterKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.CommandFilterKey
  ( CommandFilterKey
      ( ..,
        CommandFilterKey_DocumentName,
        CommandFilterKey_ExecutionStage,
        CommandFilterKey_InvokedAfter,
        CommandFilterKey_InvokedBefore,
        CommandFilterKey_Status
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype CommandFilterKey = CommandFilterKey'
  { fromCommandFilterKey ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern CommandFilterKey_DocumentName :: CommandFilterKey
pattern CommandFilterKey_DocumentName = CommandFilterKey' "DocumentName"

pattern CommandFilterKey_ExecutionStage :: CommandFilterKey
pattern CommandFilterKey_ExecutionStage = CommandFilterKey' "ExecutionStage"

pattern CommandFilterKey_InvokedAfter :: CommandFilterKey
pattern CommandFilterKey_InvokedAfter = CommandFilterKey' "InvokedAfter"

pattern CommandFilterKey_InvokedBefore :: CommandFilterKey
pattern CommandFilterKey_InvokedBefore = CommandFilterKey' "InvokedBefore"

pattern CommandFilterKey_Status :: CommandFilterKey
pattern CommandFilterKey_Status = CommandFilterKey' "Status"

{-# COMPLETE
  CommandFilterKey_DocumentName,
  CommandFilterKey_ExecutionStage,
  CommandFilterKey_InvokedAfter,
  CommandFilterKey_InvokedBefore,
  CommandFilterKey_Status,
  CommandFilterKey'
  #-}
