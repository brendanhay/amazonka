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
-- Module      : Network.AWS.Glue.Types.ConnectionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ConnectionType
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

import qualified Network.AWS.Prelude as Prelude

newtype ConnectionType = ConnectionType'
  { fromConnectionType ::
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
