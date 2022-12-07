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
-- Module      : Amazonka.CognitoIdentityProvider.Types.AdvancedSecurityModeType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.AdvancedSecurityModeType
  ( AdvancedSecurityModeType
      ( ..,
        AdvancedSecurityModeType_AUDIT,
        AdvancedSecurityModeType_ENFORCED,
        AdvancedSecurityModeType_OFF
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AdvancedSecurityModeType = AdvancedSecurityModeType'
  { fromAdvancedSecurityModeType ::
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
