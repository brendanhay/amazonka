{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.AppType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.AppType
  ( AppType
      ( AppType',
        AppTypeAwsFlowRuby,
        AppTypeJava,
        AppTypeRails,
        AppTypePhp,
        AppTypeNodejs,
        AppTypeStatic,
        AppTypeOther,
        fromAppType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype AppType = AppType' {fromAppType :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern AppTypeAwsFlowRuby :: AppType
pattern AppTypeAwsFlowRuby = AppType' "aws-flow-ruby"

pattern AppTypeJava :: AppType
pattern AppTypeJava = AppType' "java"

pattern AppTypeRails :: AppType
pattern AppTypeRails = AppType' "rails"

pattern AppTypePhp :: AppType
pattern AppTypePhp = AppType' "php"

pattern AppTypeNodejs :: AppType
pattern AppTypeNodejs = AppType' "nodejs"

pattern AppTypeStatic :: AppType
pattern AppTypeStatic = AppType' "static"

pattern AppTypeOther :: AppType
pattern AppTypeOther = AppType' "other"

{-# COMPLETE
  AppTypeAwsFlowRuby,
  AppTypeJava,
  AppTypeRails,
  AppTypePhp,
  AppTypeNodejs,
  AppTypeStatic,
  AppTypeOther,
  AppType'
  #-}
