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
-- Module      : Amazonka.MediaLive.Types.SmoothGroupSparseTrackType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.SmoothGroupSparseTrackType
  ( SmoothGroupSparseTrackType
      ( ..,
        SmoothGroupSparseTrackType_NONE,
        SmoothGroupSparseTrackType_SCTE_35,
        SmoothGroupSparseTrackType_SCTE_35_WITHOUT_SEGMENTATION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Smooth Group Sparse Track Type
newtype SmoothGroupSparseTrackType = SmoothGroupSparseTrackType'
  { fromSmoothGroupSparseTrackType ::
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

pattern SmoothGroupSparseTrackType_NONE :: SmoothGroupSparseTrackType
pattern SmoothGroupSparseTrackType_NONE = SmoothGroupSparseTrackType' "NONE"

pattern SmoothGroupSparseTrackType_SCTE_35 :: SmoothGroupSparseTrackType
pattern SmoothGroupSparseTrackType_SCTE_35 = SmoothGroupSparseTrackType' "SCTE_35"

pattern SmoothGroupSparseTrackType_SCTE_35_WITHOUT_SEGMENTATION :: SmoothGroupSparseTrackType
pattern SmoothGroupSparseTrackType_SCTE_35_WITHOUT_SEGMENTATION = SmoothGroupSparseTrackType' "SCTE_35_WITHOUT_SEGMENTATION"

{-# COMPLETE
  SmoothGroupSparseTrackType_NONE,
  SmoothGroupSparseTrackType_SCTE_35,
  SmoothGroupSparseTrackType_SCTE_35_WITHOUT_SEGMENTATION,
  SmoothGroupSparseTrackType'
  #-}
