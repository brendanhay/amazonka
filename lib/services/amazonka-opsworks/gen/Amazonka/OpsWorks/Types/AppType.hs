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
-- Module      : Amazonka.OpsWorks.Types.AppType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.AppType
  ( AppType
      ( ..,
        AppType_Aws_flow_ruby,
        AppType_Java,
        AppType_Nodejs,
        AppType_Other,
        AppType_Php,
        AppType_Rails,
        AppType_Static
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

pattern AppType_Aws_flow_ruby :: AppType
pattern AppType_Aws_flow_ruby = AppType' "aws-flow-ruby"

pattern AppType_Java :: AppType
pattern AppType_Java = AppType' "java"

pattern AppType_Nodejs :: AppType
pattern AppType_Nodejs = AppType' "nodejs"

pattern AppType_Other :: AppType
pattern AppType_Other = AppType' "other"

pattern AppType_Php :: AppType
pattern AppType_Php = AppType' "php"

pattern AppType_Rails :: AppType
pattern AppType_Rails = AppType' "rails"

pattern AppType_Static :: AppType
pattern AppType_Static = AppType' "static"

{-# COMPLETE
  AppType_Aws_flow_ruby,
  AppType_Java,
  AppType_Nodejs,
  AppType_Other,
  AppType_Php,
  AppType_Rails,
  AppType_Static,
  AppType'
  #-}
