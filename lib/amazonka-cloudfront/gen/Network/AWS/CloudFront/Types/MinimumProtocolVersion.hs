{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        SSLV3,
        TLSV1,
        TLSV12016,
        TLSV1_12016,
        TLSV1_22018,
        TLSV1_22019
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

pattern SSLV3 :: MinimumProtocolVersion
pattern SSLV3 = MinimumProtocolVersion' "SSLv3"

pattern TLSV1 :: MinimumProtocolVersion
pattern TLSV1 = MinimumProtocolVersion' "TLSv1"

pattern TLSV12016 :: MinimumProtocolVersion
pattern TLSV12016 = MinimumProtocolVersion' "TLSv1_2016"

pattern TLSV1_12016 :: MinimumProtocolVersion
pattern TLSV1_12016 = MinimumProtocolVersion' "TLSv1.1_2016"

pattern TLSV1_22018 :: MinimumProtocolVersion
pattern TLSV1_22018 = MinimumProtocolVersion' "TLSv1.2_2018"

pattern TLSV1_22019 :: MinimumProtocolVersion
pattern TLSV1_22019 = MinimumProtocolVersion' "TLSv1.2_2019"

{-# COMPLETE
  SSLV3,
  TLSV1,
  TLSV12016,
  TLSV1_12016,
  TLSV1_22018,
  TLSV1_22019,
  MinimumProtocolVersion'
  #-}
