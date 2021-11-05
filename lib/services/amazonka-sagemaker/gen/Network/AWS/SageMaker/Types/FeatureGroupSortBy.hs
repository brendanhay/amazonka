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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
import qualified Amazonka.Prelude as Prelude

newtype FeatureGroupSortBy = FeatureGroupSortBy'
  { fromFeatureGroupSortBy ::
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
