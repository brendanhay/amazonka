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
-- Module      : Amazonka.KMS.Types.ExpirationModelType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KMS.Types.ExpirationModelType
  ( ExpirationModelType
      ( ..,
        ExpirationModelType_KEY_MATERIAL_DOES_NOT_EXPIRE,
        ExpirationModelType_KEY_MATERIAL_EXPIRES
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ExpirationModelType = ExpirationModelType'
  { fromExpirationModelType ::
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

pattern ExpirationModelType_KEY_MATERIAL_DOES_NOT_EXPIRE :: ExpirationModelType
pattern ExpirationModelType_KEY_MATERIAL_DOES_NOT_EXPIRE = ExpirationModelType' "KEY_MATERIAL_DOES_NOT_EXPIRE"

pattern ExpirationModelType_KEY_MATERIAL_EXPIRES :: ExpirationModelType
pattern ExpirationModelType_KEY_MATERIAL_EXPIRES = ExpirationModelType' "KEY_MATERIAL_EXPIRES"

{-# COMPLETE
  ExpirationModelType_KEY_MATERIAL_DOES_NOT_EXPIRE,
  ExpirationModelType_KEY_MATERIAL_EXPIRES,
  ExpirationModelType'
  #-}
