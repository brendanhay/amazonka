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
-- Module      : Amazonka.MigrationHubStrategy.Types.AppType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.AppType
  ( AppType
      ( ..,
        AppType_DotNetFramework,
        AppType_IIS,
        AppType_Java,
        AppType_Oracle,
        AppType_Other,
        AppType_SQLServer
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AppType = AppType' {fromAppType :: Data.Text}
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

pattern AppType_DotNetFramework :: AppType
pattern AppType_DotNetFramework = AppType' "DotNetFramework"

pattern AppType_IIS :: AppType
pattern AppType_IIS = AppType' "IIS"

pattern AppType_Java :: AppType
pattern AppType_Java = AppType' "Java"

pattern AppType_Oracle :: AppType
pattern AppType_Oracle = AppType' "Oracle"

pattern AppType_Other :: AppType
pattern AppType_Other = AppType' "Other"

pattern AppType_SQLServer :: AppType
pattern AppType_SQLServer = AppType' "SQLServer"

{-# COMPLETE
  AppType_DotNetFramework,
  AppType_IIS,
  AppType_Java,
  AppType_Oracle,
  AppType_Other,
  AppType_SQLServer,
  AppType'
  #-}
