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
-- Module      : Amazonka.SSM.Types.CommandFilterKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.CommandFilterKey
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CommandFilterKey = CommandFilterKey'
  { fromCommandFilterKey ::
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
