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
-- Module      : Amazonka.CodeBuild.Types.ServerType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.ServerType
  ( ServerType
      ( ..,
        ServerType_BITBUCKET,
        ServerType_GITHUB,
        ServerType_GITHUB_ENTERPRISE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ServerType = ServerType'
  { fromServerType ::
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

pattern ServerType_BITBUCKET :: ServerType
pattern ServerType_BITBUCKET = ServerType' "BITBUCKET"

pattern ServerType_GITHUB :: ServerType
pattern ServerType_GITHUB = ServerType' "GITHUB"

pattern ServerType_GITHUB_ENTERPRISE :: ServerType
pattern ServerType_GITHUB_ENTERPRISE = ServerType' "GITHUB_ENTERPRISE"

{-# COMPLETE
  ServerType_BITBUCKET,
  ServerType_GITHUB,
  ServerType_GITHUB_ENTERPRISE,
  ServerType'
  #-}
