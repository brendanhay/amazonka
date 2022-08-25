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
-- Module      : Amazonka.Detective.Types.DatasourcePackageIngestState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Detective.Types.DatasourcePackageIngestState
  ( DatasourcePackageIngestState
      ( ..,
        DatasourcePackageIngestState_DISABLED,
        DatasourcePackageIngestState_STARTED,
        DatasourcePackageIngestState_STOPPED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype DatasourcePackageIngestState = DatasourcePackageIngestState'
  { fromDatasourcePackageIngestState ::
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

pattern DatasourcePackageIngestState_DISABLED :: DatasourcePackageIngestState
pattern DatasourcePackageIngestState_DISABLED = DatasourcePackageIngestState' "DISABLED"

pattern DatasourcePackageIngestState_STARTED :: DatasourcePackageIngestState
pattern DatasourcePackageIngestState_STARTED = DatasourcePackageIngestState' "STARTED"

pattern DatasourcePackageIngestState_STOPPED :: DatasourcePackageIngestState
pattern DatasourcePackageIngestState_STOPPED = DatasourcePackageIngestState' "STOPPED"

{-# COMPLETE
  DatasourcePackageIngestState_DISABLED,
  DatasourcePackageIngestState_STARTED,
  DatasourcePackageIngestState_STOPPED,
  DatasourcePackageIngestState'
  #-}
