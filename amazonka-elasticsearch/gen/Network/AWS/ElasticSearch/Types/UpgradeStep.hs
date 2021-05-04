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

import qualified Network.AWS.Prelude as Prelude

newtype UpgradeStep = UpgradeStep'
  { fromUpgradeStep ::
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
