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

import qualified Network.AWS.Prelude as Prelude

newtype AdvancedSecurityModeType = AdvancedSecurityModeType'
  { fromAdvancedSecurityModeType ::
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
