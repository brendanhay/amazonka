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
-- Module      : Amazonka.MediaLive.Types.MsSmoothH265PackagingType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.MsSmoothH265PackagingType
  ( MsSmoothH265PackagingType
      ( ..,
        MsSmoothH265PackagingType_HEV1,
        MsSmoothH265PackagingType_HVC1
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Ms Smooth H265 Packaging Type
newtype MsSmoothH265PackagingType = MsSmoothH265PackagingType'
  { fromMsSmoothH265PackagingType ::
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

pattern MsSmoothH265PackagingType_HEV1 :: MsSmoothH265PackagingType
pattern MsSmoothH265PackagingType_HEV1 = MsSmoothH265PackagingType' "HEV1"

pattern MsSmoothH265PackagingType_HVC1 :: MsSmoothH265PackagingType
pattern MsSmoothH265PackagingType_HVC1 = MsSmoothH265PackagingType' "HVC1"

{-# COMPLETE
  MsSmoothH265PackagingType_HEV1,
  MsSmoothH265PackagingType_HVC1,
  MsSmoothH265PackagingType'
  #-}
