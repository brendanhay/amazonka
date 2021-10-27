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
-- Module      : Network.AWS.ComprehendMedical.Types.ICD10CMAttributeType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ComprehendMedical.Types.ICD10CMAttributeType
  ( ICD10CMAttributeType
      ( ..,
        ICD10CMAttributeType_ACUITY,
        ICD10CMAttributeType_DIRECTION,
        ICD10CMAttributeType_QUALITY,
        ICD10CMAttributeType_QUANTITY,
        ICD10CMAttributeType_SYSTEM_ORGAN_SITE,
        ICD10CMAttributeType_TIME_EXPRESSION,
        ICD10CMAttributeType_TIME_TO_DX_NAME
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ICD10CMAttributeType = ICD10CMAttributeType'
  { fromICD10CMAttributeType ::
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

pattern ICD10CMAttributeType_ACUITY :: ICD10CMAttributeType
pattern ICD10CMAttributeType_ACUITY = ICD10CMAttributeType' "ACUITY"

pattern ICD10CMAttributeType_DIRECTION :: ICD10CMAttributeType
pattern ICD10CMAttributeType_DIRECTION = ICD10CMAttributeType' "DIRECTION"

pattern ICD10CMAttributeType_QUALITY :: ICD10CMAttributeType
pattern ICD10CMAttributeType_QUALITY = ICD10CMAttributeType' "QUALITY"

pattern ICD10CMAttributeType_QUANTITY :: ICD10CMAttributeType
pattern ICD10CMAttributeType_QUANTITY = ICD10CMAttributeType' "QUANTITY"

pattern ICD10CMAttributeType_SYSTEM_ORGAN_SITE :: ICD10CMAttributeType
pattern ICD10CMAttributeType_SYSTEM_ORGAN_SITE = ICD10CMAttributeType' "SYSTEM_ORGAN_SITE"

pattern ICD10CMAttributeType_TIME_EXPRESSION :: ICD10CMAttributeType
pattern ICD10CMAttributeType_TIME_EXPRESSION = ICD10CMAttributeType' "TIME_EXPRESSION"

pattern ICD10CMAttributeType_TIME_TO_DX_NAME :: ICD10CMAttributeType
pattern ICD10CMAttributeType_TIME_TO_DX_NAME = ICD10CMAttributeType' "TIME_TO_DX_NAME"

{-# COMPLETE
  ICD10CMAttributeType_ACUITY,
  ICD10CMAttributeType_DIRECTION,
  ICD10CMAttributeType_QUALITY,
  ICD10CMAttributeType_QUANTITY,
  ICD10CMAttributeType_SYSTEM_ORGAN_SITE,
  ICD10CMAttributeType_TIME_EXPRESSION,
  ICD10CMAttributeType_TIME_TO_DX_NAME,
  ICD10CMAttributeType'
  #-}
