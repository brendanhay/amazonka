-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.ValidityPeriodType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.ValidityPeriodType
  ( ValidityPeriodType
      ( ValidityPeriodType',
        Absolute,
        Days,
        EndDate,
        Months,
        Years
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ValidityPeriodType = ValidityPeriodType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Absolute :: ValidityPeriodType
pattern Absolute = ValidityPeriodType' "ABSOLUTE"

pattern Days :: ValidityPeriodType
pattern Days = ValidityPeriodType' "DAYS"

pattern EndDate :: ValidityPeriodType
pattern EndDate = ValidityPeriodType' "END_DATE"

pattern Months :: ValidityPeriodType
pattern Months = ValidityPeriodType' "MONTHS"

pattern Years :: ValidityPeriodType
pattern Years = ValidityPeriodType' "YEARS"

{-# COMPLETE
  Absolute,
  Days,
  EndDate,
  Months,
  Years,
  ValidityPeriodType'
  #-}
