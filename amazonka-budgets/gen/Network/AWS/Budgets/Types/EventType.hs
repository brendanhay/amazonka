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
-- Module      : Network.AWS.Budgets.Types.EventType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.EventType
  ( EventType
      ( ..,
        EventType_CREATE_ACTION,
        EventType_DELETE_ACTION,
        EventType_EXECUTE_ACTION,
        EventType_SYSTEM,
        EventType_UPDATE_ACTION
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype EventType = EventType'
  { fromEventType ::
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

pattern EventType_CREATE_ACTION :: EventType
pattern EventType_CREATE_ACTION = EventType' "CREATE_ACTION"

pattern EventType_DELETE_ACTION :: EventType
pattern EventType_DELETE_ACTION = EventType' "DELETE_ACTION"

pattern EventType_EXECUTE_ACTION :: EventType
pattern EventType_EXECUTE_ACTION = EventType' "EXECUTE_ACTION"

pattern EventType_SYSTEM :: EventType
pattern EventType_SYSTEM = EventType' "SYSTEM"

pattern EventType_UPDATE_ACTION :: EventType
pattern EventType_UPDATE_ACTION = EventType' "UPDATE_ACTION"

{-# COMPLETE
  EventType_CREATE_ACTION,
  EventType_DELETE_ACTION,
  EventType_EXECUTE_ACTION,
  EventType_SYSTEM,
  EventType_UPDATE_ACTION,
  EventType'
  #-}
