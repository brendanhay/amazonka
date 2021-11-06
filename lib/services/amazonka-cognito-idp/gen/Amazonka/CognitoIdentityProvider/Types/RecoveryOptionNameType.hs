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
-- Module      : Amazonka.CognitoIdentityProvider.Types.RecoveryOptionNameType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.RecoveryOptionNameType
  ( RecoveryOptionNameType
      ( ..,
        RecoveryOptionNameType_Admin_only,
        RecoveryOptionNameType_Verified_email,
        RecoveryOptionNameType_Verified_phone_number
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype RecoveryOptionNameType = RecoveryOptionNameType'
  { fromRecoveryOptionNameType ::
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

pattern RecoveryOptionNameType_Admin_only :: RecoveryOptionNameType
pattern RecoveryOptionNameType_Admin_only = RecoveryOptionNameType' "admin_only"

pattern RecoveryOptionNameType_Verified_email :: RecoveryOptionNameType
pattern RecoveryOptionNameType_Verified_email = RecoveryOptionNameType' "verified_email"

pattern RecoveryOptionNameType_Verified_phone_number :: RecoveryOptionNameType
pattern RecoveryOptionNameType_Verified_phone_number = RecoveryOptionNameType' "verified_phone_number"

{-# COMPLETE
  RecoveryOptionNameType_Admin_only,
  RecoveryOptionNameType_Verified_email,
  RecoveryOptionNameType_Verified_phone_number,
  RecoveryOptionNameType'
  #-}
