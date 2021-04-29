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
-- Module      : Network.AWS.MediaLive.Types.InputState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputState
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

import qualified Network.AWS.Prelude as Prelude

-- | Placeholder documentation for InputState
newtype InputState = InputState'
  { fromInputState ::
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
