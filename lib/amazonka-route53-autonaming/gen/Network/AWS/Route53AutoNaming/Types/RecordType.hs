{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.RecordType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53AutoNaming.Types.RecordType
  ( RecordType
    ( RecordType'
    , RecordTypeSrv
    , RecordTypeA
    , RecordTypeAaaa
    , RecordTypeCname
    , fromRecordType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype RecordType = RecordType'{fromRecordType :: Core.Text}
                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                       Core.Generic)
                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                         Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                         Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                         Core.FromText, Core.ToByteString, Core.ToQuery,
                                         Core.ToHeader)

pattern RecordTypeSrv :: RecordType
pattern RecordTypeSrv = RecordType' "SRV"

pattern RecordTypeA :: RecordType
pattern RecordTypeA = RecordType' "A"

pattern RecordTypeAaaa :: RecordType
pattern RecordTypeAaaa = RecordType' "AAAA"

pattern RecordTypeCname :: RecordType
pattern RecordTypeCname = RecordType' "CNAME"

{-# COMPLETE 
  RecordTypeSrv,

  RecordTypeA,

  RecordTypeAaaa,

  RecordTypeCname,
  RecordType'
  #-}
