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
-- Module      : Network.AWS.Kendra.Types.DocumentStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.DocumentStatus
  ( DocumentStatus
      ( ..,
        DocumentStatus_FAILED,
        DocumentStatus_INDEXED,
        DocumentStatus_NOT_FOUND,
        DocumentStatus_PROCESSING,
        DocumentStatus_UPDATED,
        DocumentStatus_UPDATE_FAILED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype DocumentStatus = DocumentStatus'
  { fromDocumentStatus ::
      Core.Text
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

pattern DocumentStatus_FAILED :: DocumentStatus
pattern DocumentStatus_FAILED = DocumentStatus' "FAILED"

pattern DocumentStatus_INDEXED :: DocumentStatus
pattern DocumentStatus_INDEXED = DocumentStatus' "INDEXED"

pattern DocumentStatus_NOT_FOUND :: DocumentStatus
pattern DocumentStatus_NOT_FOUND = DocumentStatus' "NOT_FOUND"

pattern DocumentStatus_PROCESSING :: DocumentStatus
pattern DocumentStatus_PROCESSING = DocumentStatus' "PROCESSING"

pattern DocumentStatus_UPDATED :: DocumentStatus
pattern DocumentStatus_UPDATED = DocumentStatus' "UPDATED"

pattern DocumentStatus_UPDATE_FAILED :: DocumentStatus
pattern DocumentStatus_UPDATE_FAILED = DocumentStatus' "UPDATE_FAILED"

{-# COMPLETE
  DocumentStatus_FAILED,
  DocumentStatus_INDEXED,
  DocumentStatus_NOT_FOUND,
  DocumentStatus_PROCESSING,
  DocumentStatus_UPDATED,
  DocumentStatus_UPDATE_FAILED,
  DocumentStatus'
  #-}
