{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype CommandFilterKey = CommandFilterKey'
  { fromCommandFilterKey ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
