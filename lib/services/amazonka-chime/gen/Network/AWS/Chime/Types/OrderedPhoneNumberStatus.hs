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
-- Module      : Network.AWS.Chime.Types.OrderedPhoneNumberStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Chime.Types.OrderedPhoneNumberStatus
  ( OrderedPhoneNumberStatus
      ( ..,
        OrderedPhoneNumberStatus_Acquired,
        OrderedPhoneNumberStatus_Failed,
        OrderedPhoneNumberStatus_Processing
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype OrderedPhoneNumberStatus = OrderedPhoneNumberStatus'
  { fromOrderedPhoneNumberStatus ::
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

pattern OrderedPhoneNumberStatus_Acquired :: OrderedPhoneNumberStatus
pattern OrderedPhoneNumberStatus_Acquired = OrderedPhoneNumberStatus' "Acquired"

pattern OrderedPhoneNumberStatus_Failed :: OrderedPhoneNumberStatus
pattern OrderedPhoneNumberStatus_Failed = OrderedPhoneNumberStatus' "Failed"

pattern OrderedPhoneNumberStatus_Processing :: OrderedPhoneNumberStatus
pattern OrderedPhoneNumberStatus_Processing = OrderedPhoneNumberStatus' "Processing"

{-# COMPLETE
  OrderedPhoneNumberStatus_Acquired,
  OrderedPhoneNumberStatus_Failed,
  OrderedPhoneNumberStatus_Processing,
  OrderedPhoneNumberStatus'
  #-}
