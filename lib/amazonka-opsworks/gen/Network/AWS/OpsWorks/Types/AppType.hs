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
        ATAWSFlowRuby,
        ATJava,
        ATNodejs,
        ATOther,
        ATPHP,
        ATRails,
        ATStatic
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AppType = AppType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern ATAWSFlowRuby :: AppType
pattern ATAWSFlowRuby = AppType' "aws-flow-ruby"

pattern ATJava :: AppType
pattern ATJava = AppType' "java"

pattern ATNodejs :: AppType
pattern ATNodejs = AppType' "nodejs"

pattern ATOther :: AppType
pattern ATOther = AppType' "other"

pattern ATPHP :: AppType
pattern ATPHP = AppType' "php"

pattern ATRails :: AppType
pattern ATRails = AppType' "rails"

pattern ATStatic :: AppType
pattern ATStatic = AppType' "static"

{-# COMPLETE
  ATAWSFlowRuby,
  ATJava,
  ATNodejs,
  ATOther,
  ATPHP,
  ATRails,
  ATStatic,
  AppType'
  #-}
