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
-- Module      : Amazonka.PaymentCryptography.Types.KeyState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptography.Types.KeyState
  ( KeyState
      ( ..,
        KeyState_CREATE_COMPLETE,
        KeyState_CREATE_IN_PROGRESS,
        KeyState_DELETE_COMPLETE,
        KeyState_DELETE_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines the state of a key
newtype KeyState = KeyState'
  { fromKeyState ::
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

pattern KeyState_CREATE_COMPLETE :: KeyState
pattern KeyState_CREATE_COMPLETE = KeyState' "CREATE_COMPLETE"

pattern KeyState_CREATE_IN_PROGRESS :: KeyState
pattern KeyState_CREATE_IN_PROGRESS = KeyState' "CREATE_IN_PROGRESS"

pattern KeyState_DELETE_COMPLETE :: KeyState
pattern KeyState_DELETE_COMPLETE = KeyState' "DELETE_COMPLETE"

pattern KeyState_DELETE_PENDING :: KeyState
pattern KeyState_DELETE_PENDING = KeyState' "DELETE_PENDING"

{-# COMPLETE
  KeyState_CREATE_COMPLETE,
  KeyState_CREATE_IN_PROGRESS,
  KeyState_DELETE_COMPLETE,
  KeyState_DELETE_PENDING,
  KeyState'
  #-}
