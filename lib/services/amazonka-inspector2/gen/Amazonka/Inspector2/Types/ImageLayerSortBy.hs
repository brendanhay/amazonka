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
-- Module      : Amazonka.Inspector2.Types.ImageLayerSortBy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.ImageLayerSortBy
  ( ImageLayerSortBy
      ( ..,
        ImageLayerSortBy_ALL,
        ImageLayerSortBy_CRITICAL,
        ImageLayerSortBy_HIGH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ImageLayerSortBy = ImageLayerSortBy'
  { fromImageLayerSortBy ::
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

pattern ImageLayerSortBy_ALL :: ImageLayerSortBy
pattern ImageLayerSortBy_ALL = ImageLayerSortBy' "ALL"

pattern ImageLayerSortBy_CRITICAL :: ImageLayerSortBy
pattern ImageLayerSortBy_CRITICAL = ImageLayerSortBy' "CRITICAL"

pattern ImageLayerSortBy_HIGH :: ImageLayerSortBy
pattern ImageLayerSortBy_HIGH = ImageLayerSortBy' "HIGH"

{-# COMPLETE
  ImageLayerSortBy_ALL,
  ImageLayerSortBy_CRITICAL,
  ImageLayerSortBy_HIGH,
  ImageLayerSortBy'
  #-}
