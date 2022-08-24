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
-- Module      : Amazonka.Proton.Types.RepositoryProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.RepositoryProvider
  ( RepositoryProvider
      ( ..,
        RepositoryProvider_BITBUCKET,
        RepositoryProvider_GITHUB,
        RepositoryProvider_GITHUB_ENTERPRISE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype RepositoryProvider = RepositoryProvider'
  { fromRepositoryProvider ::
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

pattern RepositoryProvider_BITBUCKET :: RepositoryProvider
pattern RepositoryProvider_BITBUCKET = RepositoryProvider' "BITBUCKET"

pattern RepositoryProvider_GITHUB :: RepositoryProvider
pattern RepositoryProvider_GITHUB = RepositoryProvider' "GITHUB"

pattern RepositoryProvider_GITHUB_ENTERPRISE :: RepositoryProvider
pattern RepositoryProvider_GITHUB_ENTERPRISE = RepositoryProvider' "GITHUB_ENTERPRISE"

{-# COMPLETE
  RepositoryProvider_BITBUCKET,
  RepositoryProvider_GITHUB,
  RepositoryProvider_GITHUB_ENTERPRISE,
  RepositoryProvider'
  #-}
