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

import qualified Network.AWS.Prelude as Prelude

-- | The status of a document.
newtype DocumentStatus = DocumentStatus'
  { fromDocumentStatus ::
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
