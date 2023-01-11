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
-- Module      : Amazonka.IoT.Types.ActionType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ActionType
  ( ActionType
      ( ..,
        ActionType_CONNECT,
        ActionType_PUBLISH,
        ActionType_RECEIVE,
        ActionType_SUBSCRIBE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ActionType = ActionType'
  { fromActionType ::
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
