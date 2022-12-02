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
-- Module      : Amazonka.Pinpoint.Types.AttributeType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.AttributeType
  ( AttributeType
      ( ..,
        AttributeType_AFTER,
        AttributeType_BEFORE,
        AttributeType_BETWEEN,
        AttributeType_CONTAINS,
        AttributeType_EXCLUSIVE,
        AttributeType_INCLUSIVE,
        AttributeType_ON
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AttributeType = AttributeType'
  { fromAttributeType ::
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

pattern AttributeType_AFTER :: AttributeType
pattern AttributeType_AFTER = AttributeType' "AFTER"

pattern AttributeType_BEFORE :: AttributeType
pattern AttributeType_BEFORE = AttributeType' "BEFORE"

pattern AttributeType_BETWEEN :: AttributeType
pattern AttributeType_BETWEEN = AttributeType' "BETWEEN"

pattern AttributeType_CONTAINS :: AttributeType
pattern AttributeType_CONTAINS = AttributeType' "CONTAINS"

pattern AttributeType_EXCLUSIVE :: AttributeType
pattern AttributeType_EXCLUSIVE = AttributeType' "EXCLUSIVE"

pattern AttributeType_INCLUSIVE :: AttributeType
pattern AttributeType_INCLUSIVE = AttributeType' "INCLUSIVE"

pattern AttributeType_ON :: AttributeType
pattern AttributeType_ON = AttributeType' "ON"

{-# COMPLETE
  AttributeType_AFTER,
  AttributeType_BEFORE,
  AttributeType_BETWEEN,
  AttributeType_CONTAINS,
  AttributeType_EXCLUSIVE,
  AttributeType_INCLUSIVE,
  AttributeType_ON,
  AttributeType'
  #-}
