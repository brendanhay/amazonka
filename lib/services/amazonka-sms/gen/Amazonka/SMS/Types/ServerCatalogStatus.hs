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
-- Module      : Amazonka.SMS.Types.ServerCatalogStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.ServerCatalogStatus
  ( ServerCatalogStatus
      ( ..,
        ServerCatalogStatus_AVAILABLE,
        ServerCatalogStatus_DELETED,
        ServerCatalogStatus_EXPIRED,
        ServerCatalogStatus_IMPORTING,
        ServerCatalogStatus_NOT_IMPORTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ServerCatalogStatus = ServerCatalogStatus'
  { fromServerCatalogStatus ::
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

pattern ServerCatalogStatus_AVAILABLE :: ServerCatalogStatus
pattern ServerCatalogStatus_AVAILABLE = ServerCatalogStatus' "AVAILABLE"

pattern ServerCatalogStatus_DELETED :: ServerCatalogStatus
pattern ServerCatalogStatus_DELETED = ServerCatalogStatus' "DELETED"

pattern ServerCatalogStatus_EXPIRED :: ServerCatalogStatus
pattern ServerCatalogStatus_EXPIRED = ServerCatalogStatus' "EXPIRED"

pattern ServerCatalogStatus_IMPORTING :: ServerCatalogStatus
pattern ServerCatalogStatus_IMPORTING = ServerCatalogStatus' "IMPORTING"

pattern ServerCatalogStatus_NOT_IMPORTED :: ServerCatalogStatus
pattern ServerCatalogStatus_NOT_IMPORTED = ServerCatalogStatus' "NOT_IMPORTED"

{-# COMPLETE
  ServerCatalogStatus_AVAILABLE,
  ServerCatalogStatus_DELETED,
  ServerCatalogStatus_EXPIRED,
  ServerCatalogStatus_IMPORTING,
  ServerCatalogStatus_NOT_IMPORTED,
  ServerCatalogStatus'
  #-}
