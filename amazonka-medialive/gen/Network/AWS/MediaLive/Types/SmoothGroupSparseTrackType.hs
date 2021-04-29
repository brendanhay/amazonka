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
-- Module      : Network.AWS.MediaLive.Types.SmoothGroupSparseTrackType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.SmoothGroupSparseTrackType
  ( SmoothGroupSparseTrackType
      ( ..,
        SmoothGroupSparseTrackType_NONE,
        SmoothGroupSparseTrackType_SCTE_35,
        SmoothGroupSparseTrackType_SCTE_35_WITHOUT_SEGMENTATION
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Smooth Group Sparse Track Type
newtype SmoothGroupSparseTrackType = SmoothGroupSparseTrackType'
  { fromSmoothGroupSparseTrackType ::
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
