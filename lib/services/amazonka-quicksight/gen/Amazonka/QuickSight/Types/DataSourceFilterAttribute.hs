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
-- Module      : Amazonka.QuickSight.Types.DataSourceFilterAttribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DataSourceFilterAttribute
  ( DataSourceFilterAttribute
      ( ..,
        DataSourceFilterAttribute_DATASOURCE_NAME,
        DataSourceFilterAttribute_DIRECT_QUICKSIGHT_OWNER,
        DataSourceFilterAttribute_DIRECT_QUICKSIGHT_SOLE_OWNER,
        DataSourceFilterAttribute_DIRECT_QUICKSIGHT_VIEWER_OR_OWNER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DataSourceFilterAttribute = DataSourceFilterAttribute'
  { fromDataSourceFilterAttribute ::
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

pattern DataSourceFilterAttribute_DATASOURCE_NAME :: DataSourceFilterAttribute
pattern DataSourceFilterAttribute_DATASOURCE_NAME = DataSourceFilterAttribute' "DATASOURCE_NAME"

pattern DataSourceFilterAttribute_DIRECT_QUICKSIGHT_OWNER :: DataSourceFilterAttribute
pattern DataSourceFilterAttribute_DIRECT_QUICKSIGHT_OWNER = DataSourceFilterAttribute' "DIRECT_QUICKSIGHT_OWNER"

pattern DataSourceFilterAttribute_DIRECT_QUICKSIGHT_SOLE_OWNER :: DataSourceFilterAttribute
pattern DataSourceFilterAttribute_DIRECT_QUICKSIGHT_SOLE_OWNER = DataSourceFilterAttribute' "DIRECT_QUICKSIGHT_SOLE_OWNER"

pattern DataSourceFilterAttribute_DIRECT_QUICKSIGHT_VIEWER_OR_OWNER :: DataSourceFilterAttribute
pattern DataSourceFilterAttribute_DIRECT_QUICKSIGHT_VIEWER_OR_OWNER = DataSourceFilterAttribute' "DIRECT_QUICKSIGHT_VIEWER_OR_OWNER"

{-# COMPLETE
  DataSourceFilterAttribute_DATASOURCE_NAME,
  DataSourceFilterAttribute_DIRECT_QUICKSIGHT_OWNER,
  DataSourceFilterAttribute_DIRECT_QUICKSIGHT_SOLE_OWNER,
  DataSourceFilterAttribute_DIRECT_QUICKSIGHT_VIEWER_OR_OWNER,
  DataSourceFilterAttribute'
  #-}
