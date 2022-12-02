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
-- Module      : Amazonka.Route53AutoNaming.Types.RecordType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53AutoNaming.Types.RecordType
  ( RecordType
      ( ..,
        RecordType_A,
        RecordType_AAAA,
        RecordType_CNAME,
        RecordType_SRV
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RecordType = RecordType'
  { fromRecordType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
