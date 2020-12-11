-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.DocumentSourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.DocumentSourceType
  ( DocumentSourceType
      ( DocumentSourceType',
        Original,
        WithComments
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DocumentSourceType = DocumentSourceType' Lude.Text
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

pattern Original :: DocumentSourceType
pattern Original = DocumentSourceType' "ORIGINAL"

pattern WithComments :: DocumentSourceType
pattern WithComments = DocumentSourceType' "WITH_COMMENTS"

{-# COMPLETE
  Original,
  WithComments,
  DocumentSourceType'
  #-}
