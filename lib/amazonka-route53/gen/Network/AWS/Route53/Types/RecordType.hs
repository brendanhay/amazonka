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
        A,
        Aaaa,
        Caa,
        Cname,
        MX,
        NS,
        Naptr,
        Ptr,
        Soa,
        Spf,
        Srv,
        Txt
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

pattern A :: RecordType
pattern A = RecordType' "A"

pattern Aaaa :: RecordType
pattern Aaaa = RecordType' "AAAA"

pattern Caa :: RecordType
pattern Caa = RecordType' "CAA"

pattern Cname :: RecordType
pattern Cname = RecordType' "CNAME"

pattern MX :: RecordType
pattern MX = RecordType' "MX"

pattern NS :: RecordType
pattern NS = RecordType' "NS"

pattern Naptr :: RecordType
pattern Naptr = RecordType' "NAPTR"

pattern Ptr :: RecordType
pattern Ptr = RecordType' "PTR"

pattern Soa :: RecordType
pattern Soa = RecordType' "SOA"

pattern Spf :: RecordType
pattern Spf = RecordType' "SPF"

pattern Srv :: RecordType
pattern Srv = RecordType' "SRV"

pattern Txt :: RecordType
pattern Txt = RecordType' "TXT"

{-# COMPLETE
  A,
  Aaaa,
  Caa,
  Cname,
  MX,
  NS,
  Naptr,
  Ptr,
  Soa,
  Spf,
  Srv,
  Txt,
  RecordType'
  #-}
