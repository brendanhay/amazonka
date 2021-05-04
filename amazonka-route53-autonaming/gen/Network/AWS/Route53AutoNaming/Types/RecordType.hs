{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.RecordType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.RecordType
  ( RecordType
      ( ..,
        RecordType_A,
        RecordType_AAAA,
        RecordType_CNAME,
        RecordType_SRV
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype RecordType = RecordType'
  { fromRecordType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern RecordType_A :: RecordType
pattern RecordType_A = RecordType' "A"

pattern RecordType_AAAA :: RecordType
pattern RecordType_AAAA = RecordType' "AAAA"

pattern RecordType_CNAME :: RecordType
pattern RecordType_CNAME = RecordType' "CNAME"

pattern RecordType_SRV :: RecordType
pattern RecordType_SRV = RecordType' "SRV"

{-# COMPLETE
  RecordType_A,
  RecordType_AAAA,
  RecordType_CNAME,
  RecordType_SRV,
  RecordType'
  #-}
