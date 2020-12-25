{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ProjectionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ProjectionType
  ( ProjectionType
      ( ProjectionType',
        ProjectionTypeAll,
        ProjectionTypeKeysOnly,
        ProjectionTypeInclude,
        fromProjectionType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ProjectionType = ProjectionType'
  { fromProjectionType ::
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

pattern ProjectionTypeAll :: ProjectionType
pattern ProjectionTypeAll = ProjectionType' "ALL"

pattern ProjectionTypeKeysOnly :: ProjectionType
pattern ProjectionTypeKeysOnly = ProjectionType' "KEYS_ONLY"

pattern ProjectionTypeInclude :: ProjectionType
pattern ProjectionTypeInclude = ProjectionType' "INCLUDE"

{-# COMPLETE
  ProjectionTypeAll,
  ProjectionTypeKeysOnly,
  ProjectionTypeInclude,
  ProjectionType'
  #-}
