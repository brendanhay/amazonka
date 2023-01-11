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
-- Module      : Amazonka.KMS.Types.CustomKeyStoreType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KMS.Types.CustomKeyStoreType
  ( CustomKeyStoreType
      ( ..,
        CustomKeyStoreType_AWS_CLOUDHSM,
        CustomKeyStoreType_EXTERNAL_KEY_STORE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CustomKeyStoreType = CustomKeyStoreType'
  { fromCustomKeyStoreType ::
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

pattern CustomKeyStoreType_AWS_CLOUDHSM :: CustomKeyStoreType
pattern CustomKeyStoreType_AWS_CLOUDHSM = CustomKeyStoreType' "AWS_CLOUDHSM"

pattern CustomKeyStoreType_EXTERNAL_KEY_STORE :: CustomKeyStoreType
pattern CustomKeyStoreType_EXTERNAL_KEY_STORE = CustomKeyStoreType' "EXTERNAL_KEY_STORE"

{-# COMPLETE
  CustomKeyStoreType_AWS_CLOUDHSM,
  CustomKeyStoreType_EXTERNAL_KEY_STORE,
  CustomKeyStoreType'
  #-}
