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
-- Module      : Amazonka.AlexaBusiness.Types.PhoneNumberType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.PhoneNumberType
  ( PhoneNumberType
      ( ..,
        PhoneNumberType_HOME,
        PhoneNumberType_MOBILE,
        PhoneNumberType_WORK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PhoneNumberType = PhoneNumberType'
  { fromPhoneNumberType ::
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

pattern PhoneNumberType_HOME :: PhoneNumberType
pattern PhoneNumberType_HOME = PhoneNumberType' "HOME"

pattern PhoneNumberType_MOBILE :: PhoneNumberType
pattern PhoneNumberType_MOBILE = PhoneNumberType' "MOBILE"

pattern PhoneNumberType_WORK :: PhoneNumberType
pattern PhoneNumberType_WORK = PhoneNumberType' "WORK"

{-# COMPLETE
  PhoneNumberType_HOME,
  PhoneNumberType_MOBILE,
  PhoneNumberType_WORK,
  PhoneNumberType'
  #-}
