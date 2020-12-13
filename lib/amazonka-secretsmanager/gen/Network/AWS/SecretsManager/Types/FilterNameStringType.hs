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
        Description,
        Name,
        TagKey,
        TagValue,
        All
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype FilterNameStringType = FilterNameStringType' Lude.Text
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

pattern Description :: FilterNameStringType
pattern Description = FilterNameStringType' "description"

pattern Name :: FilterNameStringType
pattern Name = FilterNameStringType' "name"

pattern TagKey :: FilterNameStringType
pattern TagKey = FilterNameStringType' "tag-key"

pattern TagValue :: FilterNameStringType
pattern TagValue = FilterNameStringType' "tag-value"

pattern All :: FilterNameStringType
pattern All = FilterNameStringType' "all"

{-# COMPLETE
  Description,
  Name,
  TagKey,
  TagValue,
  All,
  FilterNameStringType'
  #-}
