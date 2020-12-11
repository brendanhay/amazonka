-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.CommentStatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.CommentStatusType
  ( CommentStatusType
      ( CommentStatusType',
        Deleted,
        Draft,
        Published
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype CommentStatusType = CommentStatusType' Lude.Text
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

pattern Deleted :: CommentStatusType
pattern Deleted = CommentStatusType' "DELETED"

pattern Draft :: CommentStatusType
pattern Draft = CommentStatusType' "DRAFT"

pattern Published :: CommentStatusType
pattern Published = CommentStatusType' "PUBLISHED"

{-# COMPLETE
  Deleted,
  Draft,
  Published,
  CommentStatusType'
  #-}
