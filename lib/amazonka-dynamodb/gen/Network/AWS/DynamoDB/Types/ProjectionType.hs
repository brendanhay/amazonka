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
        PTAll,
        PTInclude,
        PTKeysOnly
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ProjectionType = ProjectionType' Lude.Text
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

pattern PTAll :: ProjectionType
pattern PTAll = ProjectionType' "ALL"

pattern PTInclude :: ProjectionType
pattern PTInclude = ProjectionType' "INCLUDE"

pattern PTKeysOnly :: ProjectionType
pattern PTKeysOnly = ProjectionType' "KEYS_ONLY"

{-# COMPLETE
  PTAll,
  PTInclude,
  PTKeysOnly,
  ProjectionType'
  #-}
