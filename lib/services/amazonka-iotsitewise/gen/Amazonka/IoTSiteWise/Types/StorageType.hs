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
-- Module      : Amazonka.IoTSiteWise.Types.StorageType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.StorageType
  ( StorageType
      ( ..,
        StorageType_MULTI_LAYER_STORAGE,
        StorageType_SITEWISE_DEFAULT_STORAGE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype StorageType = StorageType'
  { fromStorageType ::
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

pattern StorageType_MULTI_LAYER_STORAGE :: StorageType
pattern StorageType_MULTI_LAYER_STORAGE = StorageType' "MULTI_LAYER_STORAGE"

pattern StorageType_SITEWISE_DEFAULT_STORAGE :: StorageType
pattern StorageType_SITEWISE_DEFAULT_STORAGE = StorageType' "SITEWISE_DEFAULT_STORAGE"

{-# COMPLETE
  StorageType_MULTI_LAYER_STORAGE,
  StorageType_SITEWISE_DEFAULT_STORAGE,
  StorageType'
  #-}
