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
-- Module      : Amazonka.EMR.Types.RepoUpgradeOnBoot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.RepoUpgradeOnBoot
  ( RepoUpgradeOnBoot
      ( ..,
        RepoUpgradeOnBoot_NONE,
        RepoUpgradeOnBoot_SECURITY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RepoUpgradeOnBoot = RepoUpgradeOnBoot'
  { fromRepoUpgradeOnBoot ::
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

pattern RepoUpgradeOnBoot_NONE :: RepoUpgradeOnBoot
pattern RepoUpgradeOnBoot_NONE = RepoUpgradeOnBoot' "NONE"

pattern RepoUpgradeOnBoot_SECURITY :: RepoUpgradeOnBoot
pattern RepoUpgradeOnBoot_SECURITY = RepoUpgradeOnBoot' "SECURITY"

{-# COMPLETE
  RepoUpgradeOnBoot_NONE,
  RepoUpgradeOnBoot_SECURITY,
  RepoUpgradeOnBoot'
  #-}
