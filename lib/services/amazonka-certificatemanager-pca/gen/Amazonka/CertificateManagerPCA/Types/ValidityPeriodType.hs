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
-- Module      : Amazonka.CertificateManagerPCA.Types.ValidityPeriodType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Types.ValidityPeriodType
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ValidityPeriodType = ValidityPeriodType'
  { fromValidityPeriodType ::
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
