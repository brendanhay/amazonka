{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AttachmentsSourceKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.AttachmentsSourceKey
  ( AttachmentsSourceKey
    ( AttachmentsSourceKey'
    , AttachmentsSourceKeySourceUrl
    , AttachmentsSourceKeyS3FileUrl
    , AttachmentsSourceKeyAttachmentReference
    , fromAttachmentsSourceKey
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype AttachmentsSourceKey = AttachmentsSourceKey'{fromAttachmentsSourceKey
                                                     :: Core.Text}
                                 deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                 Core.Generic)
                                 deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                   Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                   Core.FromJSON, Core.ToXML, Core.FromXML,
                                                   Core.ToText, Core.FromText, Core.ToByteString,
                                                   Core.ToQuery, Core.ToHeader)

pattern AttachmentsSourceKeySourceUrl :: AttachmentsSourceKey
pattern AttachmentsSourceKeySourceUrl = AttachmentsSourceKey' "SourceUrl"

pattern AttachmentsSourceKeyS3FileUrl :: AttachmentsSourceKey
pattern AttachmentsSourceKeyS3FileUrl = AttachmentsSourceKey' "S3FileUrl"

pattern AttachmentsSourceKeyAttachmentReference :: AttachmentsSourceKey
pattern AttachmentsSourceKeyAttachmentReference = AttachmentsSourceKey' "AttachmentReference"

{-# COMPLETE 
  AttachmentsSourceKeySourceUrl,

  AttachmentsSourceKeyS3FileUrl,

  AttachmentsSourceKeyAttachmentReference,
  AttachmentsSourceKey'
  #-}
