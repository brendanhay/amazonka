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
-- Module      : Amazonka.QuickSight.Types.DataSetFilterAttribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DataSetFilterAttribute
  ( DataSetFilterAttribute
      ( ..,
        DataSetFilterAttribute_DATASET_NAME,
        DataSetFilterAttribute_DIRECT_QUICKSIGHT_OWNER,
        DataSetFilterAttribute_DIRECT_QUICKSIGHT_SOLE_OWNER,
        DataSetFilterAttribute_DIRECT_QUICKSIGHT_VIEWER_OR_OWNER,
        DataSetFilterAttribute_QUICKSIGHT_OWNER,
        DataSetFilterAttribute_QUICKSIGHT_VIEWER_OR_OWNER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype DataSetFilterAttribute = DataSetFilterAttribute'
  { fromDataSetFilterAttribute ::
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

pattern DataSetFilterAttribute_DATASET_NAME :: DataSetFilterAttribute
pattern DataSetFilterAttribute_DATASET_NAME = DataSetFilterAttribute' "DATASET_NAME"

pattern DataSetFilterAttribute_DIRECT_QUICKSIGHT_OWNER :: DataSetFilterAttribute
pattern DataSetFilterAttribute_DIRECT_QUICKSIGHT_OWNER = DataSetFilterAttribute' "DIRECT_QUICKSIGHT_OWNER"

pattern DataSetFilterAttribute_DIRECT_QUICKSIGHT_SOLE_OWNER :: DataSetFilterAttribute
pattern DataSetFilterAttribute_DIRECT_QUICKSIGHT_SOLE_OWNER = DataSetFilterAttribute' "DIRECT_QUICKSIGHT_SOLE_OWNER"

pattern DataSetFilterAttribute_DIRECT_QUICKSIGHT_VIEWER_OR_OWNER :: DataSetFilterAttribute
pattern DataSetFilterAttribute_DIRECT_QUICKSIGHT_VIEWER_OR_OWNER = DataSetFilterAttribute' "DIRECT_QUICKSIGHT_VIEWER_OR_OWNER"

pattern DataSetFilterAttribute_QUICKSIGHT_OWNER :: DataSetFilterAttribute
pattern DataSetFilterAttribute_QUICKSIGHT_OWNER = DataSetFilterAttribute' "QUICKSIGHT_OWNER"

pattern DataSetFilterAttribute_QUICKSIGHT_VIEWER_OR_OWNER :: DataSetFilterAttribute
pattern DataSetFilterAttribute_QUICKSIGHT_VIEWER_OR_OWNER = DataSetFilterAttribute' "QUICKSIGHT_VIEWER_OR_OWNER"

{-# COMPLETE
  DataSetFilterAttribute_DATASET_NAME,
  DataSetFilterAttribute_DIRECT_QUICKSIGHT_OWNER,
  DataSetFilterAttribute_DIRECT_QUICKSIGHT_SOLE_OWNER,
  DataSetFilterAttribute_DIRECT_QUICKSIGHT_VIEWER_OR_OWNER,
  DataSetFilterAttribute_QUICKSIGHT_OWNER,
  DataSetFilterAttribute_QUICKSIGHT_VIEWER_OR_OWNER,
  DataSetFilterAttribute'
  #-}
