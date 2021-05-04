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
-- Module      : Network.AWS.OpsWorks.Types.AppType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.AppType
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

import qualified Network.AWS.Prelude as Prelude

newtype AppType = AppType'
  { fromAppType ::
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
