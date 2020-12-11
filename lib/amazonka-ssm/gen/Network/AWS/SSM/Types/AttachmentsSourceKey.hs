-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AttachmentsSourceKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AttachmentsSourceKey
  ( AttachmentsSourceKey
      ( AttachmentsSourceKey',
        AttachmentReference,
        S3FileURL,
        SourceURL
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AttachmentsSourceKey = AttachmentsSourceKey' Lude.Text
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

pattern AttachmentReference :: AttachmentsSourceKey
pattern AttachmentReference = AttachmentsSourceKey' "AttachmentReference"

pattern S3FileURL :: AttachmentsSourceKey
pattern S3FileURL = AttachmentsSourceKey' "S3FileUrl"

pattern SourceURL :: AttachmentsSourceKey
pattern SourceURL = AttachmentsSourceKey' "SourceUrl"

{-# COMPLETE
  AttachmentReference,
  S3FileURL,
  SourceURL,
  AttachmentsSourceKey'
  #-}
