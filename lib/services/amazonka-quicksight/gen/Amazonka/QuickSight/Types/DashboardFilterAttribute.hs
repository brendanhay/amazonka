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
-- Module      : Amazonka.QuickSight.Types.DashboardFilterAttribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DashboardFilterAttribute
  ( DashboardFilterAttribute
      ( ..,
        DashboardFilterAttribute_DASHBOARD_NAME,
        DashboardFilterAttribute_DIRECT_QUICKSIGHT_OWNER,
        DashboardFilterAttribute_DIRECT_QUICKSIGHT_SOLE_OWNER,
        DashboardFilterAttribute_DIRECT_QUICKSIGHT_VIEWER_OR_OWNER,
        DashboardFilterAttribute_QUICKSIGHT_OWNER,
        DashboardFilterAttribute_QUICKSIGHT_USER,
        DashboardFilterAttribute_QUICKSIGHT_VIEWER_OR_OWNER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DashboardFilterAttribute = DashboardFilterAttribute'
  { fromDashboardFilterAttribute ::
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

pattern DashboardFilterAttribute_DASHBOARD_NAME :: DashboardFilterAttribute
pattern DashboardFilterAttribute_DASHBOARD_NAME = DashboardFilterAttribute' "DASHBOARD_NAME"

pattern DashboardFilterAttribute_DIRECT_QUICKSIGHT_OWNER :: DashboardFilterAttribute
pattern DashboardFilterAttribute_DIRECT_QUICKSIGHT_OWNER = DashboardFilterAttribute' "DIRECT_QUICKSIGHT_OWNER"

pattern DashboardFilterAttribute_DIRECT_QUICKSIGHT_SOLE_OWNER :: DashboardFilterAttribute
pattern DashboardFilterAttribute_DIRECT_QUICKSIGHT_SOLE_OWNER = DashboardFilterAttribute' "DIRECT_QUICKSIGHT_SOLE_OWNER"

pattern DashboardFilterAttribute_DIRECT_QUICKSIGHT_VIEWER_OR_OWNER :: DashboardFilterAttribute
pattern DashboardFilterAttribute_DIRECT_QUICKSIGHT_VIEWER_OR_OWNER = DashboardFilterAttribute' "DIRECT_QUICKSIGHT_VIEWER_OR_OWNER"

pattern DashboardFilterAttribute_QUICKSIGHT_OWNER :: DashboardFilterAttribute
pattern DashboardFilterAttribute_QUICKSIGHT_OWNER = DashboardFilterAttribute' "QUICKSIGHT_OWNER"

pattern DashboardFilterAttribute_QUICKSIGHT_USER :: DashboardFilterAttribute
pattern DashboardFilterAttribute_QUICKSIGHT_USER = DashboardFilterAttribute' "QUICKSIGHT_USER"

pattern DashboardFilterAttribute_QUICKSIGHT_VIEWER_OR_OWNER :: DashboardFilterAttribute
pattern DashboardFilterAttribute_QUICKSIGHT_VIEWER_OR_OWNER = DashboardFilterAttribute' "QUICKSIGHT_VIEWER_OR_OWNER"

{-# COMPLETE
  DashboardFilterAttribute_DASHBOARD_NAME,
  DashboardFilterAttribute_DIRECT_QUICKSIGHT_OWNER,
  DashboardFilterAttribute_DIRECT_QUICKSIGHT_SOLE_OWNER,
  DashboardFilterAttribute_DIRECT_QUICKSIGHT_VIEWER_OR_OWNER,
  DashboardFilterAttribute_QUICKSIGHT_OWNER,
  DashboardFilterAttribute_QUICKSIGHT_USER,
  DashboardFilterAttribute_QUICKSIGHT_VIEWER_OR_OWNER,
  DashboardFilterAttribute'
  #-}
