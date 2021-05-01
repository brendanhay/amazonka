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
-- Module      : Network.AWS.EMR.Types.RepoUpgradeOnBoot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.RepoUpgradeOnBoot
  ( RepoUpgradeOnBoot
      ( ..,
        RepoUpgradeOnBoot_NONE,
        RepoUpgradeOnBoot_SECURITY
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype RepoUpgradeOnBoot = RepoUpgradeOnBoot'
  { fromRepoUpgradeOnBoot ::
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

pattern RepoUpgradeOnBoot_NONE :: RepoUpgradeOnBoot
pattern RepoUpgradeOnBoot_NONE = RepoUpgradeOnBoot' "NONE"

pattern RepoUpgradeOnBoot_SECURITY :: RepoUpgradeOnBoot
pattern RepoUpgradeOnBoot_SECURITY = RepoUpgradeOnBoot' "SECURITY"

{-# COMPLETE
  RepoUpgradeOnBoot_NONE,
  RepoUpgradeOnBoot_SECURITY,
  RepoUpgradeOnBoot'
  #-}
