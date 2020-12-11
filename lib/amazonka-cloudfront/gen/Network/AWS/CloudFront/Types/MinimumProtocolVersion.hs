-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.MinimumProtocolVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.MinimumProtocolVersion
  ( MinimumProtocolVersion
      ( MinimumProtocolVersion',
        MPVSSLV3,
        MPVTLSV1,
        MPVTLSV12016,
        MPVTLSV1_12016,
        MPVTLSV1_22018,
        MPVTLSV1_22019
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype MinimumProtocolVersion = MinimumProtocolVersion' Lude.Text
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

pattern MPVSSLV3 :: MinimumProtocolVersion
pattern MPVSSLV3 = MinimumProtocolVersion' "SSLv3"

pattern MPVTLSV1 :: MinimumProtocolVersion
pattern MPVTLSV1 = MinimumProtocolVersion' "TLSv1"

pattern MPVTLSV12016 :: MinimumProtocolVersion
pattern MPVTLSV12016 = MinimumProtocolVersion' "TLSv1_2016"

pattern MPVTLSV1_12016 :: MinimumProtocolVersion
pattern MPVTLSV1_12016 = MinimumProtocolVersion' "TLSv1.1_2016"

pattern MPVTLSV1_22018 :: MinimumProtocolVersion
pattern MPVTLSV1_22018 = MinimumProtocolVersion' "TLSv1.2_2018"

pattern MPVTLSV1_22019 :: MinimumProtocolVersion
pattern MPVTLSV1_22019 = MinimumProtocolVersion' "TLSv1.2_2019"

{-# COMPLETE
  MPVSSLV3,
  MPVTLSV1,
  MPVTLSV12016,
  MPVTLSV1_12016,
  MPVTLSV1_22018,
  MPVTLSV1_22019,
  MinimumProtocolVersion'
  #-}
