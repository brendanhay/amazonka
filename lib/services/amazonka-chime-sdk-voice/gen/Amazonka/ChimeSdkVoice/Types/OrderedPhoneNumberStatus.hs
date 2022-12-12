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
-- Module      : Amazonka.ChimeSdkVoice.Types.OrderedPhoneNumberStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.OrderedPhoneNumberStatus
  ( OrderedPhoneNumberStatus
      ( ..,
        OrderedPhoneNumberStatus_Acquired,
        OrderedPhoneNumberStatus_Failed,
        OrderedPhoneNumberStatus_Processing
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OrderedPhoneNumberStatus = OrderedPhoneNumberStatus'
  { fromOrderedPhoneNumberStatus ::
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
