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
-- Module      : Network.AWS.ElasticSearch.Types.UpgradeStep
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.UpgradeStep
  ( UpgradeStep
      ( ..,
        UpgradeStep_PRE_UPGRADE_CHECK,
        UpgradeStep_SNAPSHOT,
        UpgradeStep_UPGRADE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype UpgradeStep = UpgradeStep'
  { fromUpgradeStep ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern UpgradeStep_PRE_UPGRADE_CHECK :: UpgradeStep
pattern UpgradeStep_PRE_UPGRADE_CHECK = UpgradeStep' "PRE_UPGRADE_CHECK"

pattern UpgradeStep_SNAPSHOT :: UpgradeStep
pattern UpgradeStep_SNAPSHOT = UpgradeStep' "SNAPSHOT"

pattern UpgradeStep_UPGRADE :: UpgradeStep
pattern UpgradeStep_UPGRADE = UpgradeStep' "UPGRADE"

{-# COMPLETE
  UpgradeStep_PRE_UPGRADE_CHECK,
  UpgradeStep_SNAPSHOT,
  UpgradeStep_UPGRADE,
  UpgradeStep'
  #-}
