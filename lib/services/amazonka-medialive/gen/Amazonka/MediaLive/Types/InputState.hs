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
-- Module      : Amazonka.MediaLive.Types.InputState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.InputState
  ( InputState
      ( ..,
        InputState_ATTACHED,
        InputState_CREATING,
        InputState_DELETED,
        InputState_DELETING,
        InputState_DETACHED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Placeholder documentation for InputState
newtype InputState = InputState'
  { fromInputState ::
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

pattern InputState_ATTACHED :: InputState
pattern InputState_ATTACHED = InputState' "ATTACHED"

pattern InputState_CREATING :: InputState
pattern InputState_CREATING = InputState' "CREATING"

pattern InputState_DELETED :: InputState
pattern InputState_DELETED = InputState' "DELETED"

pattern InputState_DELETING :: InputState
pattern InputState_DELETING = InputState' "DELETING"

pattern InputState_DETACHED :: InputState
pattern InputState_DETACHED = InputState' "DETACHED"

{-# COMPLETE
  InputState_ATTACHED,
  InputState_CREATING,
  InputState_DELETED,
  InputState_DELETING,
  InputState_DETACHED,
  InputState'
  #-}
