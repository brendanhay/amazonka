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
-- Module      : Amazonka.SageMaker.Types.ListEdgeDeploymentPlansSortBy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ListEdgeDeploymentPlansSortBy
  ( ListEdgeDeploymentPlansSortBy
      ( ..,
        ListEdgeDeploymentPlansSortBy_CREATION_TIME,
        ListEdgeDeploymentPlansSortBy_DEVICE_FLEET_NAME,
        ListEdgeDeploymentPlansSortBy_LAST_MODIFIED_TIME,
        ListEdgeDeploymentPlansSortBy_NAME
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ListEdgeDeploymentPlansSortBy = ListEdgeDeploymentPlansSortBy'
  { fromListEdgeDeploymentPlansSortBy ::
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

pattern ListEdgeDeploymentPlansSortBy_CREATION_TIME :: ListEdgeDeploymentPlansSortBy
pattern ListEdgeDeploymentPlansSortBy_CREATION_TIME = ListEdgeDeploymentPlansSortBy' "CREATION_TIME"

pattern ListEdgeDeploymentPlansSortBy_DEVICE_FLEET_NAME :: ListEdgeDeploymentPlansSortBy
pattern ListEdgeDeploymentPlansSortBy_DEVICE_FLEET_NAME = ListEdgeDeploymentPlansSortBy' "DEVICE_FLEET_NAME"

pattern ListEdgeDeploymentPlansSortBy_LAST_MODIFIED_TIME :: ListEdgeDeploymentPlansSortBy
pattern ListEdgeDeploymentPlansSortBy_LAST_MODIFIED_TIME = ListEdgeDeploymentPlansSortBy' "LAST_MODIFIED_TIME"

pattern ListEdgeDeploymentPlansSortBy_NAME :: ListEdgeDeploymentPlansSortBy
pattern ListEdgeDeploymentPlansSortBy_NAME = ListEdgeDeploymentPlansSortBy' "NAME"

{-# COMPLETE
  ListEdgeDeploymentPlansSortBy_CREATION_TIME,
  ListEdgeDeploymentPlansSortBy_DEVICE_FLEET_NAME,
  ListEdgeDeploymentPlansSortBy_LAST_MODIFIED_TIME,
  ListEdgeDeploymentPlansSortBy_NAME,
  ListEdgeDeploymentPlansSortBy'
  #-}
