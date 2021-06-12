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
-- Module      : Network.AWS.SSM.Types.DocumentFilterKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentFilterKey
  ( DocumentFilterKey
      ( ..,
        DocumentFilterKey_DocumentType,
        DocumentFilterKey_Name,
        DocumentFilterKey_Owner,
        DocumentFilterKey_PlatformTypes
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype DocumentFilterKey = DocumentFilterKey'
  { fromDocumentFilterKey ::
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

pattern DocumentFilterKey_DocumentType :: DocumentFilterKey
pattern DocumentFilterKey_DocumentType = DocumentFilterKey' "DocumentType"

pattern DocumentFilterKey_Name :: DocumentFilterKey
pattern DocumentFilterKey_Name = DocumentFilterKey' "Name"

pattern DocumentFilterKey_Owner :: DocumentFilterKey
pattern DocumentFilterKey_Owner = DocumentFilterKey' "Owner"

pattern DocumentFilterKey_PlatformTypes :: DocumentFilterKey
pattern DocumentFilterKey_PlatformTypes = DocumentFilterKey' "PlatformTypes"

{-# COMPLETE
  DocumentFilterKey_DocumentType,
  DocumentFilterKey_Name,
  DocumentFilterKey_Owner,
  DocumentFilterKey_PlatformTypes,
  DocumentFilterKey'
  #-}
