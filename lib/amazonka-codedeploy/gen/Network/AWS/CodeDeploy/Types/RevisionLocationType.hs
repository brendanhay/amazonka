{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.RevisionLocationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.RevisionLocationType
  ( RevisionLocationType
      ( RevisionLocationType',
        AppSpecContent,
        GitHub,
        S3,
        String
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype RevisionLocationType = RevisionLocationType' Lude.Text
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

pattern AppSpecContent :: RevisionLocationType
pattern AppSpecContent = RevisionLocationType' "AppSpecContent"

pattern GitHub :: RevisionLocationType
pattern GitHub = RevisionLocationType' "GitHub"

pattern S3 :: RevisionLocationType
pattern S3 = RevisionLocationType' "S3"

pattern String :: RevisionLocationType
pattern String = RevisionLocationType' "String"

{-# COMPLETE
  AppSpecContent,
  GitHub,
  S3,
  String,
  RevisionLocationType'
  #-}
