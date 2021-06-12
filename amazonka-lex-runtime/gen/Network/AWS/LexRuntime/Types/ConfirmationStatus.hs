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
-- Module      : Network.AWS.LexRuntime.Types.ConfirmationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.ConfirmationStatus
  ( ConfirmationStatus
      ( ..,
        ConfirmationStatus_Confirmed,
        ConfirmationStatus_Denied,
        ConfirmationStatus_None
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ConfirmationStatus = ConfirmationStatus'
  { fromConfirmationStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
