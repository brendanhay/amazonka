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
-- Module      : Network.AWS.IoT.Types.ActionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ActionType
  ( ActionType
      ( ..,
        ActionType_CONNECT,
        ActionType_PUBLISH,
        ActionType_RECEIVE,
        ActionType_SUBSCRIBE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ActionType = ActionType'
  { fromActionType ::
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

pattern ActionType_CONNECT :: ActionType
pattern ActionType_CONNECT = ActionType' "CONNECT"

pattern ActionType_PUBLISH :: ActionType
pattern ActionType_PUBLISH = ActionType' "PUBLISH"

pattern ActionType_RECEIVE :: ActionType
pattern ActionType_RECEIVE = ActionType' "RECEIVE"

pattern ActionType_SUBSCRIBE :: ActionType
pattern ActionType_SUBSCRIBE = ActionType' "SUBSCRIBE"

{-# COMPLETE
  ActionType_CONNECT,
  ActionType_PUBLISH,
  ActionType_RECEIVE,
  ActionType_SUBSCRIBE,
  ActionType'
  #-}
