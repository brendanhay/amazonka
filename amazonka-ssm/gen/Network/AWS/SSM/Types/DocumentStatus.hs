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
-- Module      : Network.AWS.SSM.Types.DocumentStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentStatus
  ( DocumentStatus
      ( ..,
        DocumentStatus_Active,
        DocumentStatus_Creating,
        DocumentStatus_Deleting,
        DocumentStatus_Failed,
        DocumentStatus_Updating
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | The status of a document.
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

pattern DocumentStatus_Active :: DocumentStatus
pattern DocumentStatus_Active = DocumentStatus' "Active"

pattern DocumentStatus_Creating :: DocumentStatus
pattern DocumentStatus_Creating = DocumentStatus' "Creating"

pattern DocumentStatus_Deleting :: DocumentStatus
pattern DocumentStatus_Deleting = DocumentStatus' "Deleting"

pattern DocumentStatus_Failed :: DocumentStatus
pattern DocumentStatus_Failed = DocumentStatus' "Failed"

pattern DocumentStatus_Updating :: DocumentStatus
pattern DocumentStatus_Updating = DocumentStatus' "Updating"

{-# COMPLETE
  DocumentStatus_Active,
  DocumentStatus_Creating,
  DocumentStatus_Deleting,
  DocumentStatus_Failed,
  DocumentStatus_Updating,
  DocumentStatus'
  #-}
