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
-- Module      : Amazonka.Glue.Types.ConnectionType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.ConnectionType
  ( ConnectionType
      ( ..,
        ConnectionType_CUSTOM,
        ConnectionType_JDBC,
        ConnectionType_KAFKA,
        ConnectionType_MARKETPLACE,
        ConnectionType_MONGODB,
        ConnectionType_NETWORK,
        ConnectionType_SFTP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ConnectionType = ConnectionType'
  { fromConnectionType ::
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

pattern ConnectionType_CUSTOM :: ConnectionType
pattern ConnectionType_CUSTOM = ConnectionType' "CUSTOM"

pattern ConnectionType_JDBC :: ConnectionType
pattern ConnectionType_JDBC = ConnectionType' "JDBC"

pattern ConnectionType_KAFKA :: ConnectionType
pattern ConnectionType_KAFKA = ConnectionType' "KAFKA"

pattern ConnectionType_MARKETPLACE :: ConnectionType
pattern ConnectionType_MARKETPLACE = ConnectionType' "MARKETPLACE"

pattern ConnectionType_MONGODB :: ConnectionType
pattern ConnectionType_MONGODB = ConnectionType' "MONGODB"

pattern ConnectionType_NETWORK :: ConnectionType
pattern ConnectionType_NETWORK = ConnectionType' "NETWORK"

pattern ConnectionType_SFTP :: ConnectionType
pattern ConnectionType_SFTP = ConnectionType' "SFTP"

{-# COMPLETE
  ConnectionType_CUSTOM,
  ConnectionType_JDBC,
  ConnectionType_KAFKA,
  ConnectionType_MARKETPLACE,
  ConnectionType_MONGODB,
  ConnectionType_NETWORK,
  ConnectionType_SFTP,
  ConnectionType'
  #-}
