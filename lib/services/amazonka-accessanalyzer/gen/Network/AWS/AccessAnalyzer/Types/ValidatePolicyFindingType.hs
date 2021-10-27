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
-- Module      : Network.AWS.AccessAnalyzer.Types.ValidatePolicyFindingType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AccessAnalyzer.Types.ValidatePolicyFindingType
  ( ValidatePolicyFindingType
      ( ..,
        ValidatePolicyFindingType_ERROR,
        ValidatePolicyFindingType_SECURITY_WARNING,
        ValidatePolicyFindingType_SUGGESTION,
        ValidatePolicyFindingType_WARNING
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ValidatePolicyFindingType = ValidatePolicyFindingType'
  { fromValidatePolicyFindingType ::
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

pattern ValidatePolicyFindingType_ERROR :: ValidatePolicyFindingType
pattern ValidatePolicyFindingType_ERROR = ValidatePolicyFindingType' "ERROR"

pattern ValidatePolicyFindingType_SECURITY_WARNING :: ValidatePolicyFindingType
pattern ValidatePolicyFindingType_SECURITY_WARNING = ValidatePolicyFindingType' "SECURITY_WARNING"

pattern ValidatePolicyFindingType_SUGGESTION :: ValidatePolicyFindingType
pattern ValidatePolicyFindingType_SUGGESTION = ValidatePolicyFindingType' "SUGGESTION"

pattern ValidatePolicyFindingType_WARNING :: ValidatePolicyFindingType
pattern ValidatePolicyFindingType_WARNING = ValidatePolicyFindingType' "WARNING"

{-# COMPLETE
  ValidatePolicyFindingType_ERROR,
  ValidatePolicyFindingType_SECURITY_WARNING,
  ValidatePolicyFindingType_SUGGESTION,
  ValidatePolicyFindingType_WARNING,
  ValidatePolicyFindingType'
  #-}
