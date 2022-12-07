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
-- Module      : Amazonka.ResilienceHub.Types.HaArchitecture
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.HaArchitecture
  ( HaArchitecture
      ( ..,
        HaArchitecture_BackupAndRestore,
        HaArchitecture_MultiSite,
        HaArchitecture_NoRecoveryPlan,
        HaArchitecture_PilotLight,
        HaArchitecture_WarmStandby
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype HaArchitecture = HaArchitecture'
  { fromHaArchitecture ::
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

pattern HaArchitecture_BackupAndRestore :: HaArchitecture
pattern HaArchitecture_BackupAndRestore = HaArchitecture' "BackupAndRestore"

pattern HaArchitecture_MultiSite :: HaArchitecture
pattern HaArchitecture_MultiSite = HaArchitecture' "MultiSite"

pattern HaArchitecture_NoRecoveryPlan :: HaArchitecture
pattern HaArchitecture_NoRecoveryPlan = HaArchitecture' "NoRecoveryPlan"

pattern HaArchitecture_PilotLight :: HaArchitecture
pattern HaArchitecture_PilotLight = HaArchitecture' "PilotLight"

pattern HaArchitecture_WarmStandby :: HaArchitecture
pattern HaArchitecture_WarmStandby = HaArchitecture' "WarmStandby"

{-# COMPLETE
  HaArchitecture_BackupAndRestore,
  HaArchitecture_MultiSite,
  HaArchitecture_NoRecoveryPlan,
  HaArchitecture_PilotLight,
  HaArchitecture_WarmStandby,
  HaArchitecture'
  #-}
