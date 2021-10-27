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
-- Module      : Network.AWS.OpenSearch.Types.UpgradeStep
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpenSearch.Types.UpgradeStep
  ( UpgradeStep
      ( ..,
        UpgradeStep_PRE_UPGRADE_CHECK,
        UpgradeStep_SNAPSHOT,
        UpgradeStep_UPGRADE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype UpgradeStep = UpgradeStep'
  { fromUpgradeStep ::
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
