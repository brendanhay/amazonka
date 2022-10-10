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
-- Module      : Amazonka.LexRuntime.Types.ConfirmationStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexRuntime.Types.ConfirmationStatus
  ( ConfirmationStatus
      ( ..,
        ConfirmationStatus_Confirmed,
        ConfirmationStatus_Denied,
        ConfirmationStatus_None
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ConfirmationStatus = ConfirmationStatus'
  { fromConfirmationStatus ::
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

pattern ConfirmationStatus_Confirmed :: ConfirmationStatus
pattern ConfirmationStatus_Confirmed = ConfirmationStatus' "Confirmed"

pattern ConfirmationStatus_Denied :: ConfirmationStatus
pattern ConfirmationStatus_Denied = ConfirmationStatus' "Denied"

pattern ConfirmationStatus_None :: ConfirmationStatus
pattern ConfirmationStatus_None = ConfirmationStatus' "None"

{-# COMPLETE
  ConfirmationStatus_Confirmed,
  ConfirmationStatus_Denied,
  ConfirmationStatus_None,
  ConfirmationStatus'
  #-}
