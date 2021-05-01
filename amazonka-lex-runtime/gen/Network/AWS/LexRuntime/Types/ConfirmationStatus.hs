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

import qualified Network.AWS.Prelude as Prelude

newtype ConfirmationStatus = ConfirmationStatus'
  { fromConfirmationStatus ::
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
