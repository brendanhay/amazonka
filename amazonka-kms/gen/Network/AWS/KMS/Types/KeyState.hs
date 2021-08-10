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
-- Module      : Network.AWS.KMS.Types.KeyState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.KeyState
  ( KeyState
      ( ..,
        KeyState_Disabled,
        KeyState_Enabled,
        KeyState_PendingDeletion,
        KeyState_PendingImport,
        KeyState_Unavailable
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype KeyState = KeyState'
  { fromKeyState ::
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

pattern KeyState_Disabled :: KeyState
pattern KeyState_Disabled = KeyState' "Disabled"

pattern KeyState_Enabled :: KeyState
pattern KeyState_Enabled = KeyState' "Enabled"

pattern KeyState_PendingDeletion :: KeyState
pattern KeyState_PendingDeletion = KeyState' "PendingDeletion"

pattern KeyState_PendingImport :: KeyState
pattern KeyState_PendingImport = KeyState' "PendingImport"

pattern KeyState_Unavailable :: KeyState
pattern KeyState_Unavailable = KeyState' "Unavailable"

{-# COMPLETE
  KeyState_Disabled,
  KeyState_Enabled,
  KeyState_PendingDeletion,
  KeyState_PendingImport,
  KeyState_Unavailable,
  KeyState'
  #-}
