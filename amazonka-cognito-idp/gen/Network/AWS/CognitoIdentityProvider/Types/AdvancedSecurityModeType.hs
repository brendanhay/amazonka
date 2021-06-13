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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AdvancedSecurityModeType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AdvancedSecurityModeType
  ( AdvancedSecurityModeType
      ( ..,
        AdvancedSecurityModeType_AUDIT,
        AdvancedSecurityModeType_ENFORCED,
        AdvancedSecurityModeType_OFF
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype AdvancedSecurityModeType = AdvancedSecurityModeType'
  { fromAdvancedSecurityModeType ::
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

pattern AdvancedSecurityModeType_AUDIT :: AdvancedSecurityModeType
pattern AdvancedSecurityModeType_AUDIT = AdvancedSecurityModeType' "AUDIT"

pattern AdvancedSecurityModeType_ENFORCED :: AdvancedSecurityModeType
pattern AdvancedSecurityModeType_ENFORCED = AdvancedSecurityModeType' "ENFORCED"

pattern AdvancedSecurityModeType_OFF :: AdvancedSecurityModeType
pattern AdvancedSecurityModeType_OFF = AdvancedSecurityModeType' "OFF"

{-# COMPLETE
  AdvancedSecurityModeType_AUDIT,
  AdvancedSecurityModeType_ENFORCED,
  AdvancedSecurityModeType_OFF,
  AdvancedSecurityModeType'
  #-}
