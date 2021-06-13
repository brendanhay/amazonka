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
-- Module      : Network.AWS.AlexaBusiness.Types.PhoneNumberType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.PhoneNumberType
  ( PhoneNumberType
      ( ..,
        PhoneNumberType_HOME,
        PhoneNumberType_MOBILE,
        PhoneNumberType_WORK
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype PhoneNumberType = PhoneNumberType'
  { fromPhoneNumberType ::
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
