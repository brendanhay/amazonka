{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.RecordType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.RecordType
  ( RecordType
      ( RecordType',
        Soa,
        A,
        Txt,
        NS,
        Cname,
        MX,
        Naptr,
        Ptr,
        Srv,
        Spf,
        Aaaa,
        Caa
      ),
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal

newtype RecordType = RecordType' Lude.Text
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

pattern Soa :: RecordType
pattern Soa = RecordType' "SOA"

pattern A :: RecordType
pattern A = RecordType' "A"

pattern Txt :: RecordType
pattern Txt = RecordType' "TXT"

pattern NS :: RecordType
pattern NS = RecordType' "NS"

pattern Cname :: RecordType
pattern Cname = RecordType' "CNAME"

pattern MX :: RecordType
pattern MX = RecordType' "MX"

pattern Naptr :: RecordType
pattern Naptr = RecordType' "NAPTR"

pattern Ptr :: RecordType
pattern Ptr = RecordType' "PTR"

pattern Srv :: RecordType
pattern Srv = RecordType' "SRV"

pattern Spf :: RecordType
pattern Spf = RecordType' "SPF"

pattern Aaaa :: RecordType
pattern Aaaa = RecordType' "AAAA"

pattern Caa :: RecordType
pattern Caa = RecordType' "CAA"

{-# COMPLETE
  Soa,
  A,
  Txt,
  NS,
  Cname,
  MX,
  Naptr,
  Ptr,
  Srv,
  Spf,
  Aaaa,
  Caa,
  RecordType'
  #-}
