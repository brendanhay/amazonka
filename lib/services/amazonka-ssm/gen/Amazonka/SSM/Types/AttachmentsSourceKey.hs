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
-- Module      : Amazonka.SSM.Types.AttachmentsSourceKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.AttachmentsSourceKey
  ( AttachmentsSourceKey
      ( ..,
        AttachmentsSourceKey_AttachmentReference,
        AttachmentsSourceKey_S3FileUrl,
        AttachmentsSourceKey_SourceUrl
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AttachmentsSourceKey = AttachmentsSourceKey'
  { fromAttachmentsSourceKey ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
