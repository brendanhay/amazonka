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
-- Module      : Amazonka.Glue.Types.CatalogEncryptionMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.CatalogEncryptionMode
  ( CatalogEncryptionMode
      ( ..,
        CatalogEncryptionMode_DISABLED,
        CatalogEncryptionMode_SSE_KMS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CatalogEncryptionMode = CatalogEncryptionMode'
  { fromCatalogEncryptionMode ::
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

pattern CatalogEncryptionMode_DISABLED :: CatalogEncryptionMode
pattern CatalogEncryptionMode_DISABLED = CatalogEncryptionMode' "DISABLED"

pattern CatalogEncryptionMode_SSE_KMS :: CatalogEncryptionMode
pattern CatalogEncryptionMode_SSE_KMS = CatalogEncryptionMode' "SSE-KMS"

{-# COMPLETE
  CatalogEncryptionMode_DISABLED,
  CatalogEncryptionMode_SSE_KMS,
  CatalogEncryptionMode'
  #-}
