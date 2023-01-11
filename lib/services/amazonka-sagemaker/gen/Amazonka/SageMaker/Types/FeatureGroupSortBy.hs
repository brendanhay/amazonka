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
-- Module      : Amazonka.SageMaker.Types.FeatureGroupSortBy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.FeatureGroupSortBy
  ( FeatureGroupSortBy
      ( ..,
        FeatureGroupSortBy_CreationTime,
        FeatureGroupSortBy_FeatureGroupStatus,
        FeatureGroupSortBy_Name,
        FeatureGroupSortBy_OfflineStoreStatus
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FeatureGroupSortBy = FeatureGroupSortBy'
  { fromFeatureGroupSortBy ::
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

pattern FeatureGroupSortBy_CreationTime :: FeatureGroupSortBy
pattern FeatureGroupSortBy_CreationTime = FeatureGroupSortBy' "CreationTime"

pattern FeatureGroupSortBy_FeatureGroupStatus :: FeatureGroupSortBy
pattern FeatureGroupSortBy_FeatureGroupStatus = FeatureGroupSortBy' "FeatureGroupStatus"

pattern FeatureGroupSortBy_Name :: FeatureGroupSortBy
pattern FeatureGroupSortBy_Name = FeatureGroupSortBy' "Name"

pattern FeatureGroupSortBy_OfflineStoreStatus :: FeatureGroupSortBy
pattern FeatureGroupSortBy_OfflineStoreStatus = FeatureGroupSortBy' "OfflineStoreStatus"

{-# COMPLETE
  FeatureGroupSortBy_CreationTime,
  FeatureGroupSortBy_FeatureGroupStatus,
  FeatureGroupSortBy_Name,
  FeatureGroupSortBy_OfflineStoreStatus,
  FeatureGroupSortBy'
  #-}
