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
-- Module      : Amazonka.MigrationHubStrategy.Types.ServerOsType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.ServerOsType
  ( ServerOsType
      ( ..,
        ServerOsType_AmazonLinux,
        ServerOsType_EndOfSupportWindowsServer,
        ServerOsType_Other,
        ServerOsType_Redhat,
        ServerOsType_WindowsServer
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ServerOsType = ServerOsType'
  { fromServerOsType ::
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

pattern ServerOsType_AmazonLinux :: ServerOsType
pattern ServerOsType_AmazonLinux = ServerOsType' "AmazonLinux"

pattern ServerOsType_EndOfSupportWindowsServer :: ServerOsType
pattern ServerOsType_EndOfSupportWindowsServer = ServerOsType' "EndOfSupportWindowsServer"

pattern ServerOsType_Other :: ServerOsType
pattern ServerOsType_Other = ServerOsType' "Other"

pattern ServerOsType_Redhat :: ServerOsType
pattern ServerOsType_Redhat = ServerOsType' "Redhat"

pattern ServerOsType_WindowsServer :: ServerOsType
pattern ServerOsType_WindowsServer = ServerOsType' "WindowsServer"

{-# COMPLETE
  ServerOsType_AmazonLinux,
  ServerOsType_EndOfSupportWindowsServer,
  ServerOsType_Other,
  ServerOsType_Redhat,
  ServerOsType_WindowsServer,
  ServerOsType'
  #-}
