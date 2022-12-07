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
-- Module      : Amazonka.SSM.Types.SessionFilterKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.SessionFilterKey
  ( SessionFilterKey
      ( ..,
        SessionFilterKey_InvokedAfter,
        SessionFilterKey_InvokedBefore,
        SessionFilterKey_Owner,
        SessionFilterKey_SessionId,
        SessionFilterKey_Status,
        SessionFilterKey_Target
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SessionFilterKey = SessionFilterKey'
  { fromSessionFilterKey ::
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

pattern SessionFilterKey_InvokedAfter :: SessionFilterKey
pattern SessionFilterKey_InvokedAfter = SessionFilterKey' "InvokedAfter"

pattern SessionFilterKey_InvokedBefore :: SessionFilterKey
pattern SessionFilterKey_InvokedBefore = SessionFilterKey' "InvokedBefore"

pattern SessionFilterKey_Owner :: SessionFilterKey
pattern SessionFilterKey_Owner = SessionFilterKey' "Owner"

pattern SessionFilterKey_SessionId :: SessionFilterKey
pattern SessionFilterKey_SessionId = SessionFilterKey' "SessionId"

pattern SessionFilterKey_Status :: SessionFilterKey
pattern SessionFilterKey_Status = SessionFilterKey' "Status"

pattern SessionFilterKey_Target :: SessionFilterKey
pattern SessionFilterKey_Target = SessionFilterKey' "Target"

{-# COMPLETE
  SessionFilterKey_InvokedAfter,
  SessionFilterKey_InvokedBefore,
  SessionFilterKey_Owner,
  SessionFilterKey_SessionId,
  SessionFilterKey_Status,
  SessionFilterKey_Target,
  SessionFilterKey'
  #-}
