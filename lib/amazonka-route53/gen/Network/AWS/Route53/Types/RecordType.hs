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
        RecordTypeSoa,
        RecordTypeA,
        RecordTypeTxt,
        RecordTypeNS,
        RecordTypeCname,
        RecordTypeMX,
        RecordTypeNaptr,
        RecordTypePtr,
        RecordTypeSrv,
        RecordTypeSpf,
        RecordTypeAaaa,
        RecordTypeCaa,
        fromRecordType
      ),
  )
where

import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53.Internal as Types

newtype RecordType = RecordType' {fromRecordType :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern RecordTypeSoa :: RecordType
pattern RecordTypeSoa = RecordType' "SOA"

pattern RecordTypeA :: RecordType
pattern RecordTypeA = RecordType' "A"

pattern RecordTypeTxt :: RecordType
pattern RecordTypeTxt = RecordType' "TXT"

pattern RecordTypeNS :: RecordType
pattern RecordTypeNS = RecordType' "NS"

pattern RecordTypeCname :: RecordType
pattern RecordTypeCname = RecordType' "CNAME"

pattern RecordTypeMX :: RecordType
pattern RecordTypeMX = RecordType' "MX"

pattern RecordTypeNaptr :: RecordType
pattern RecordTypeNaptr = RecordType' "NAPTR"

pattern RecordTypePtr :: RecordType
pattern RecordTypePtr = RecordType' "PTR"

pattern RecordTypeSrv :: RecordType
pattern RecordTypeSrv = RecordType' "SRV"

pattern RecordTypeSpf :: RecordType
pattern RecordTypeSpf = RecordType' "SPF"

pattern RecordTypeAaaa :: RecordType
pattern RecordTypeAaaa = RecordType' "AAAA"

pattern RecordTypeCaa :: RecordType
pattern RecordTypeCaa = RecordType' "CAA"

{-# COMPLETE
  RecordTypeSoa,
  RecordTypeA,
  RecordTypeTxt,
  RecordTypeNS,
  RecordTypeCname,
  RecordTypeMX,
  RecordTypeNaptr,
  RecordTypePtr,
  RecordTypeSrv,
  RecordTypeSpf,
  RecordTypeAaaa,
  RecordTypeCaa,
  RecordType'
  #-}
