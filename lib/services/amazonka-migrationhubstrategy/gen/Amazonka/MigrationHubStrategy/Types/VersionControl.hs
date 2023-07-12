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
-- Module      : Amazonka.MigrationHubStrategy.Types.VersionControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.VersionControl
  ( VersionControl
      ( ..,
        VersionControl_AZURE_DEVOPS_GIT,
        VersionControl_GITHUB,
        VersionControl_GITHUB_ENTERPRISE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype VersionControl = VersionControl'
  { fromVersionControl ::
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

pattern VersionControl_AZURE_DEVOPS_GIT :: VersionControl
pattern VersionControl_AZURE_DEVOPS_GIT = VersionControl' "AZURE_DEVOPS_GIT"

pattern VersionControl_GITHUB :: VersionControl
pattern VersionControl_GITHUB = VersionControl' "GITHUB"

pattern VersionControl_GITHUB_ENTERPRISE :: VersionControl
pattern VersionControl_GITHUB_ENTERPRISE = VersionControl' "GITHUB_ENTERPRISE"

{-# COMPLETE
  VersionControl_AZURE_DEVOPS_GIT,
  VersionControl_GITHUB,
  VersionControl_GITHUB_ENTERPRISE,
  VersionControl'
  #-}
