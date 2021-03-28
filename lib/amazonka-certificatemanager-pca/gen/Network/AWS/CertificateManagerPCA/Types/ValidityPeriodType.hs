{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.ValidityPeriodType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManagerPCA.Types.ValidityPeriodType
  ( ValidityPeriodType
    ( ValidityPeriodType'
    , ValidityPeriodTypeEndDate
    , ValidityPeriodTypeAbsolute
    , ValidityPeriodTypeDays
    , ValidityPeriodTypeMonths
    , ValidityPeriodTypeYears
    , fromValidityPeriodType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ValidityPeriodType = ValidityPeriodType'{fromValidityPeriodType
                                                 :: Core.Text}
                               deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                               Core.Generic)
                               deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                 Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                 Core.FromJSON, Core.ToXML, Core.FromXML,
                                                 Core.ToText, Core.FromText, Core.ToByteString,
                                                 Core.ToQuery, Core.ToHeader)

pattern ValidityPeriodTypeEndDate :: ValidityPeriodType
pattern ValidityPeriodTypeEndDate = ValidityPeriodType' "END_DATE"

pattern ValidityPeriodTypeAbsolute :: ValidityPeriodType
pattern ValidityPeriodTypeAbsolute = ValidityPeriodType' "ABSOLUTE"

pattern ValidityPeriodTypeDays :: ValidityPeriodType
pattern ValidityPeriodTypeDays = ValidityPeriodType' "DAYS"

pattern ValidityPeriodTypeMonths :: ValidityPeriodType
pattern ValidityPeriodTypeMonths = ValidityPeriodType' "MONTHS"

pattern ValidityPeriodTypeYears :: ValidityPeriodType
pattern ValidityPeriodTypeYears = ValidityPeriodType' "YEARS"

{-# COMPLETE 
  ValidityPeriodTypeEndDate,

  ValidityPeriodTypeAbsolute,

  ValidityPeriodTypeDays,

  ValidityPeriodTypeMonths,

  ValidityPeriodTypeYears,
  ValidityPeriodType'
  #-}
