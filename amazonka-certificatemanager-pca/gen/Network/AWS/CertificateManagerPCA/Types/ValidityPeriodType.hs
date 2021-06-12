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
-- Module      : Network.AWS.CertificateManagerPCA.Types.ValidityPeriodType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.ValidityPeriodType
  ( ValidityPeriodType
      ( ..,
        ValidityPeriodType_ABSOLUTE,
        ValidityPeriodType_DAYS,
        ValidityPeriodType_END_DATE,
        ValidityPeriodType_MONTHS,
        ValidityPeriodType_YEARS
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ValidityPeriodType = ValidityPeriodType'
  { fromValidityPeriodType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern ValidityPeriodType_ABSOLUTE :: ValidityPeriodType
pattern ValidityPeriodType_ABSOLUTE = ValidityPeriodType' "ABSOLUTE"

pattern ValidityPeriodType_DAYS :: ValidityPeriodType
pattern ValidityPeriodType_DAYS = ValidityPeriodType' "DAYS"

pattern ValidityPeriodType_END_DATE :: ValidityPeriodType
pattern ValidityPeriodType_END_DATE = ValidityPeriodType' "END_DATE"

pattern ValidityPeriodType_MONTHS :: ValidityPeriodType
pattern ValidityPeriodType_MONTHS = ValidityPeriodType' "MONTHS"

pattern ValidityPeriodType_YEARS :: ValidityPeriodType
pattern ValidityPeriodType_YEARS = ValidityPeriodType' "YEARS"

{-# COMPLETE
  ValidityPeriodType_ABSOLUTE,
  ValidityPeriodType_DAYS,
  ValidityPeriodType_END_DATE,
  ValidityPeriodType_MONTHS,
  ValidityPeriodType_YEARS,
  ValidityPeriodType'
  #-}
