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
-- Module      : Network.AWS.Pinpoint.Types.AttributeType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.AttributeType
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

import qualified Network.AWS.Core as Core

newtype AttributeType = AttributeType'
  { fromAttributeType ::
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
