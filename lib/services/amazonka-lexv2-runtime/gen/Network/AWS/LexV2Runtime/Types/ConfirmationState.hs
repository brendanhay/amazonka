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
-- Module      : Network.AWS.LexV2Runtime.Types.ConfirmationState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Runtime.Types.ConfirmationState
  ( ConfirmationState
      ( ..,
        ConfirmationState_Confirmed,
        ConfirmationState_Denied,
        ConfirmationState_None
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ConfirmationState = ConfirmationState'
  { fromConfirmationState ::
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

pattern ConfirmationState_Confirmed :: ConfirmationState
pattern ConfirmationState_Confirmed = ConfirmationState' "Confirmed"

pattern ConfirmationState_Denied :: ConfirmationState
pattern ConfirmationState_Denied = ConfirmationState' "Denied"

pattern ConfirmationState_None :: ConfirmationState
pattern ConfirmationState_None = ConfirmationState' "None"

{-# COMPLETE
  ConfirmationState_Confirmed,
  ConfirmationState_Denied,
  ConfirmationState_None,
  ConfirmationState'
  #-}
