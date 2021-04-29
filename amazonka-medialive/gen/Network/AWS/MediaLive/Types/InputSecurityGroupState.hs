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
-- Module      : Network.AWS.MediaLive.Types.InputSecurityGroupState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputSecurityGroupState
  ( InputSecurityGroupState
      ( ..,
        InputSecurityGroupState_DELETED,
        InputSecurityGroupState_IDLE,
        InputSecurityGroupState_IN_USE,
        InputSecurityGroupState_UPDATING
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Placeholder documentation for InputSecurityGroupState
newtype InputSecurityGroupState = InputSecurityGroupState'
  { fromInputSecurityGroupState ::
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
