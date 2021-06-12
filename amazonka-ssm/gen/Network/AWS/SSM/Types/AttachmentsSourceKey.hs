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
-- Module      : Network.AWS.SSM.Types.AttachmentsSourceKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AttachmentsSourceKey
  ( AttachmentsSourceKey
      ( ..,
        AttachmentsSourceKey_AttachmentReference,
        AttachmentsSourceKey_S3FileUrl,
        AttachmentsSourceKey_SourceUrl
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype AttachmentsSourceKey = AttachmentsSourceKey'
  { fromAttachmentsSourceKey ::
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

pattern AttachmentsSourceKey_AttachmentReference :: AttachmentsSourceKey
pattern AttachmentsSourceKey_AttachmentReference = AttachmentsSourceKey' "AttachmentReference"

pattern AttachmentsSourceKey_S3FileUrl :: AttachmentsSourceKey
pattern AttachmentsSourceKey_S3FileUrl = AttachmentsSourceKey' "S3FileUrl"

pattern AttachmentsSourceKey_SourceUrl :: AttachmentsSourceKey
pattern AttachmentsSourceKey_SourceUrl = AttachmentsSourceKey' "SourceUrl"

{-# COMPLETE
  AttachmentsSourceKey_AttachmentReference,
  AttachmentsSourceKey_S3FileUrl,
  AttachmentsSourceKey_SourceUrl,
  AttachmentsSourceKey'
  #-}
