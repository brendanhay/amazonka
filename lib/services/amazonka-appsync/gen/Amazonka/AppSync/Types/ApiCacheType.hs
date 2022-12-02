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
-- Module      : Amazonka.AppSync.Types.ApiCacheType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.ApiCacheType
  ( ApiCacheType
      ( ..,
        ApiCacheType_LARGE,
        ApiCacheType_LARGE_12X,
        ApiCacheType_LARGE_2X,
        ApiCacheType_LARGE_4X,
        ApiCacheType_LARGE_8X,
        ApiCacheType_MEDIUM,
        ApiCacheType_R4_2XLARGE,
        ApiCacheType_R4_4XLARGE,
        ApiCacheType_R4_8XLARGE,
        ApiCacheType_R4_LARGE,
        ApiCacheType_R4_XLARGE,
        ApiCacheType_SMALL,
        ApiCacheType_T2_MEDIUM,
        ApiCacheType_T2_SMALL,
        ApiCacheType_XLARGE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ApiCacheType = ApiCacheType'
  { fromApiCacheType ::
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

pattern ApiCacheType_LARGE :: ApiCacheType
pattern ApiCacheType_LARGE = ApiCacheType' "LARGE"

pattern ApiCacheType_LARGE_12X :: ApiCacheType
pattern ApiCacheType_LARGE_12X = ApiCacheType' "LARGE_12X"

pattern ApiCacheType_LARGE_2X :: ApiCacheType
pattern ApiCacheType_LARGE_2X = ApiCacheType' "LARGE_2X"

pattern ApiCacheType_LARGE_4X :: ApiCacheType
pattern ApiCacheType_LARGE_4X = ApiCacheType' "LARGE_4X"

pattern ApiCacheType_LARGE_8X :: ApiCacheType
pattern ApiCacheType_LARGE_8X = ApiCacheType' "LARGE_8X"

pattern ApiCacheType_MEDIUM :: ApiCacheType
pattern ApiCacheType_MEDIUM = ApiCacheType' "MEDIUM"

pattern ApiCacheType_R4_2XLARGE :: ApiCacheType
pattern ApiCacheType_R4_2XLARGE = ApiCacheType' "R4_2XLARGE"

pattern ApiCacheType_R4_4XLARGE :: ApiCacheType
pattern ApiCacheType_R4_4XLARGE = ApiCacheType' "R4_4XLARGE"

pattern ApiCacheType_R4_8XLARGE :: ApiCacheType
pattern ApiCacheType_R4_8XLARGE = ApiCacheType' "R4_8XLARGE"

pattern ApiCacheType_R4_LARGE :: ApiCacheType
pattern ApiCacheType_R4_LARGE = ApiCacheType' "R4_LARGE"

pattern ApiCacheType_R4_XLARGE :: ApiCacheType
pattern ApiCacheType_R4_XLARGE = ApiCacheType' "R4_XLARGE"

pattern ApiCacheType_SMALL :: ApiCacheType
pattern ApiCacheType_SMALL = ApiCacheType' "SMALL"

pattern ApiCacheType_T2_MEDIUM :: ApiCacheType
pattern ApiCacheType_T2_MEDIUM = ApiCacheType' "T2_MEDIUM"

pattern ApiCacheType_T2_SMALL :: ApiCacheType
pattern ApiCacheType_T2_SMALL = ApiCacheType' "T2_SMALL"

pattern ApiCacheType_XLARGE :: ApiCacheType
pattern ApiCacheType_XLARGE = ApiCacheType' "XLARGE"

{-# COMPLETE
  ApiCacheType_LARGE,
  ApiCacheType_LARGE_12X,
  ApiCacheType_LARGE_2X,
  ApiCacheType_LARGE_4X,
  ApiCacheType_LARGE_8X,
  ApiCacheType_MEDIUM,
  ApiCacheType_R4_2XLARGE,
  ApiCacheType_R4_4XLARGE,
  ApiCacheType_R4_8XLARGE,
  ApiCacheType_R4_LARGE,
  ApiCacheType_R4_XLARGE,
  ApiCacheType_SMALL,
  ApiCacheType_T2_MEDIUM,
  ApiCacheType_T2_SMALL,
  ApiCacheType_XLARGE,
  ApiCacheType'
  #-}
