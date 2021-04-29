{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype AttachmentsSourceKey = AttachmentsSourceKey'
  { fromAttachmentsSourceKey ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
