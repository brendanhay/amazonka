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
-- Module      : Amazonka.AppFlow.Types.WriteOperationType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.WriteOperationType
  ( WriteOperationType
      ( ..,
        WriteOperationType_DELETE,
        WriteOperationType_INSERT,
        WriteOperationType_UPDATE,
        WriteOperationType_UPSERT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The possible write operations in the destination connector. When this
-- value is not provided, this defaults to the @INSERT@ operation.
newtype WriteOperationType = WriteOperationType'
  { fromWriteOperationType ::
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

pattern WriteOperationType_DELETE :: WriteOperationType
pattern WriteOperationType_DELETE = WriteOperationType' "DELETE"

pattern WriteOperationType_INSERT :: WriteOperationType
pattern WriteOperationType_INSERT = WriteOperationType' "INSERT"

pattern WriteOperationType_UPDATE :: WriteOperationType
pattern WriteOperationType_UPDATE = WriteOperationType' "UPDATE"

pattern WriteOperationType_UPSERT :: WriteOperationType
pattern WriteOperationType_UPSERT = WriteOperationType' "UPSERT"

{-# COMPLETE
  WriteOperationType_DELETE,
  WriteOperationType_INSERT,
  WriteOperationType_UPDATE,
  WriteOperationType_UPSERT,
  WriteOperationType'
  #-}
