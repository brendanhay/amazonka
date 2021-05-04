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
-- Module      : Network.AWS.SSM.Types.SessionFilterKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.SessionFilterKey
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

import qualified Network.AWS.Prelude as Prelude

newtype SessionFilterKey = SessionFilterKey'
  { fromSessionFilterKey ::
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
