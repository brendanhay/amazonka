{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.Types.FilterNameStringType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecretsManager.Types.FilterNameStringType
  ( FilterNameStringType
      ( FilterNameStringType',
        FilterNameStringTypeDescription,
        FilterNameStringTypeName,
        FilterNameStringTypeTagKey,
        FilterNameStringTypeTagValue,
        FilterNameStringTypeAll,
        fromFilterNameStringType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype FilterNameStringType = FilterNameStringType'
  { fromFilterNameStringType ::
      Core.Text
  }
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

pattern FilterNameStringTypeDescription :: FilterNameStringType
pattern FilterNameStringTypeDescription = FilterNameStringType' "description"

pattern FilterNameStringTypeName :: FilterNameStringType
pattern FilterNameStringTypeName = FilterNameStringType' "name"

pattern FilterNameStringTypeTagKey :: FilterNameStringType
pattern FilterNameStringTypeTagKey = FilterNameStringType' "tag-key"

pattern FilterNameStringTypeTagValue :: FilterNameStringType
pattern FilterNameStringTypeTagValue = FilterNameStringType' "tag-value"

pattern FilterNameStringTypeAll :: FilterNameStringType
pattern FilterNameStringTypeAll = FilterNameStringType' "all"

{-# COMPLETE
  FilterNameStringTypeDescription,
  FilterNameStringTypeName,
  FilterNameStringTypeTagKey,
  FilterNameStringTypeTagValue,
  FilterNameStringTypeAll,
  FilterNameStringType'
  #-}
