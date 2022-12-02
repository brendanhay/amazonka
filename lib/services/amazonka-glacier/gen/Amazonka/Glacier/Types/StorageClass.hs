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
-- Module      : Amazonka.Glacier.Types.StorageClass
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glacier.Types.StorageClass
  ( StorageClass
      ( ..,
        StorageClass_REDUCED_REDUNDANCY,
        StorageClass_STANDARD,
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

pattern StorageClass_REDUCED_REDUNDANCY :: StorageClass
pattern StorageClass_REDUCED_REDUNDANCY = StorageClass' "REDUCED_REDUNDANCY"

pattern StorageClass_STANDARD :: StorageClass
pattern StorageClass_STANDARD = StorageClass' "STANDARD"

pattern StorageClass_STANDARD_IA :: StorageClass
pattern StorageClass_STANDARD_IA = StorageClass' "STANDARD_IA"

{-# COMPLETE
  StorageClass_REDUCED_REDUNDANCY,
  StorageClass_STANDARD,
  StorageClass_STANDARD_IA,
  StorageClass'
  #-}
