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
-- Module      : Amazonka.CodeDeploy.Types.RevisionLocationType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.RevisionLocationType
  ( RevisionLocationType
      ( ..,
        RevisionLocationType_AppSpecContent,
        RevisionLocationType_GitHub,
        RevisionLocationType_S3,
        RevisionLocationType_String
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype RevisionLocationType = RevisionLocationType'
  { fromRevisionLocationType ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
