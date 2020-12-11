-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.SharedResourceSortByType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.SharedResourceSortByType
  ( SharedResourceSortByType
      ( SharedResourceSortByType',
        ARN,
        ModifiedTime
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype SharedResourceSortByType = SharedResourceSortByType' Lude.Text
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

pattern ARN :: SharedResourceSortByType
pattern ARN = SharedResourceSortByType' "ARN"

pattern ModifiedTime :: SharedResourceSortByType
pattern ModifiedTime = SharedResourceSortByType' "MODIFIED_TIME"

{-# COMPLETE
  ARN,
  ModifiedTime,
  SharedResourceSortByType'
  #-}
