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
-- Module      : Amazonka.Rekognition.Types.FaceSearchSortBy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.FaceSearchSortBy
  ( FaceSearchSortBy
      ( ..,
        FaceSearchSortBy_INDEX,
        FaceSearchSortBy_TIMESTAMP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype FaceSearchSortBy = FaceSearchSortBy'
  { fromFaceSearchSortBy ::
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

pattern FaceSearchSortBy_INDEX :: FaceSearchSortBy
pattern FaceSearchSortBy_INDEX = FaceSearchSortBy' "INDEX"

pattern FaceSearchSortBy_TIMESTAMP :: FaceSearchSortBy
pattern FaceSearchSortBy_TIMESTAMP = FaceSearchSortBy' "TIMESTAMP"

{-# COMPLETE
  FaceSearchSortBy_INDEX,
  FaceSearchSortBy_TIMESTAMP,
  FaceSearchSortBy'
  #-}
