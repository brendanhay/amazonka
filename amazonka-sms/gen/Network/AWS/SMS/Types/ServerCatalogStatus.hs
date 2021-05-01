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
-- Module      : Network.AWS.SMS.Types.ServerCatalogStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ServerCatalogStatus
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

import qualified Network.AWS.Prelude as Prelude

newtype ServerCatalogStatus = ServerCatalogStatus'
  { fromServerCatalogStatus ::
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
