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
-- Module      : Network.AWS.CodeDeploy.Types.RevisionLocationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.RevisionLocationType
  ( RevisionLocationType
      ( ..,
        RevisionLocationType_AppSpecContent,
        RevisionLocationType_GitHub,
        RevisionLocationType_S3,
        RevisionLocationType_String
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype RevisionLocationType = RevisionLocationType'
  { fromRevisionLocationType ::
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

pattern RevisionLocationType_AppSpecContent :: RevisionLocationType
pattern RevisionLocationType_AppSpecContent = RevisionLocationType' "AppSpecContent"

pattern RevisionLocationType_GitHub :: RevisionLocationType
pattern RevisionLocationType_GitHub = RevisionLocationType' "GitHub"

pattern RevisionLocationType_S3 :: RevisionLocationType
pattern RevisionLocationType_S3 = RevisionLocationType' "S3"

pattern RevisionLocationType_String :: RevisionLocationType
pattern RevisionLocationType_String = RevisionLocationType' "String"

{-# COMPLETE
  RevisionLocationType_AppSpecContent,
  RevisionLocationType_GitHub,
  RevisionLocationType_S3,
  RevisionLocationType_String,
  RevisionLocationType'
  #-}
