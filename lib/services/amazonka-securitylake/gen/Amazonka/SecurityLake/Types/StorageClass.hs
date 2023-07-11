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
-- Module      : Amazonka.SecurityLake.Types.StorageClass
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.StorageClass
  ( StorageClass
      ( ..,
        StorageClass_DEEP_ARCHIVE,
        StorageClass_EXPIRE,
        StorageClass_GLACIER,
        StorageClass_GLACIER_IR,
        StorageClass_INTELLIGENT_TIERING,
        StorageClass_ONEZONE_IA,
        StorageClass_STANDARD_IA
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StorageClass = StorageClass'
  { fromStorageClass ::
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

pattern StorageClass_DEEP_ARCHIVE :: StorageClass
pattern StorageClass_DEEP_ARCHIVE = StorageClass' "DEEP_ARCHIVE"

pattern StorageClass_EXPIRE :: StorageClass
pattern StorageClass_EXPIRE = StorageClass' "EXPIRE"

pattern StorageClass_GLACIER :: StorageClass
pattern StorageClass_GLACIER = StorageClass' "GLACIER"

pattern StorageClass_GLACIER_IR :: StorageClass
pattern StorageClass_GLACIER_IR = StorageClass' "GLACIER_IR"

pattern StorageClass_INTELLIGENT_TIERING :: StorageClass
pattern StorageClass_INTELLIGENT_TIERING = StorageClass' "INTELLIGENT_TIERING"

pattern StorageClass_ONEZONE_IA :: StorageClass
pattern StorageClass_ONEZONE_IA = StorageClass' "ONEZONE_IA"

pattern StorageClass_STANDARD_IA :: StorageClass
pattern StorageClass_STANDARD_IA = StorageClass' "STANDARD_IA"

{-# COMPLETE
  StorageClass_DEEP_ARCHIVE,
  StorageClass_EXPIRE,
  StorageClass_GLACIER,
  StorageClass_GLACIER_IR,
  StorageClass_INTELLIGENT_TIERING,
  StorageClass_ONEZONE_IA,
  StorageClass_STANDARD_IA,
  StorageClass'
  #-}
