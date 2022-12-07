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
-- Module      : Amazonka.MediaLive.Types.InputSecurityGroupState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.InputSecurityGroupState
  ( InputSecurityGroupState
      ( ..,
        InputSecurityGroupState_DELETED,
        InputSecurityGroupState_IDLE,
        InputSecurityGroupState_IN_USE,
        InputSecurityGroupState_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Placeholder documentation for InputSecurityGroupState
newtype InputSecurityGroupState = InputSecurityGroupState'
  { fromInputSecurityGroupState ::
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

pattern InputSecurityGroupState_DELETED :: InputSecurityGroupState
pattern InputSecurityGroupState_DELETED = InputSecurityGroupState' "DELETED"

pattern InputSecurityGroupState_IDLE :: InputSecurityGroupState
pattern InputSecurityGroupState_IDLE = InputSecurityGroupState' "IDLE"

pattern InputSecurityGroupState_IN_USE :: InputSecurityGroupState
pattern InputSecurityGroupState_IN_USE = InputSecurityGroupState' "IN_USE"

pattern InputSecurityGroupState_UPDATING :: InputSecurityGroupState
pattern InputSecurityGroupState_UPDATING = InputSecurityGroupState' "UPDATING"

{-# COMPLETE
  InputSecurityGroupState_DELETED,
  InputSecurityGroupState_IDLE,
  InputSecurityGroupState_IN_USE,
  InputSecurityGroupState_UPDATING,
  InputSecurityGroupState'
  #-}
