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

import qualified Network.AWS.Core as Core

newtype RecordType = RecordType'
  { fromRecordType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
