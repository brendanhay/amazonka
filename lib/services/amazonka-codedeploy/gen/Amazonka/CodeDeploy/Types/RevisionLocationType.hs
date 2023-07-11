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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RevisionLocationType = RevisionLocationType'
  { fromRevisionLocationType ::
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
