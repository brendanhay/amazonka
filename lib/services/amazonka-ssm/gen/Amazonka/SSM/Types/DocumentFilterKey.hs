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
-- Module      : Amazonka.SSM.Types.DocumentFilterKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.DocumentFilterKey
  ( DocumentFilterKey
      ( ..,
        DocumentFilterKey_DocumentType,
        DocumentFilterKey_Name,
        DocumentFilterKey_Owner,
        DocumentFilterKey_PlatformTypes
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DocumentFilterKey = DocumentFilterKey'
  { fromDocumentFilterKey ::
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
