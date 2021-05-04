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
-- Module      : Network.AWS.KMS.Types.ExpirationModelType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.ExpirationModelType
  ( ExpirationModelType
      ( ..,
        ExpirationModelType_KEY_MATERIAL_DOES_NOT_EXPIRE,
        ExpirationModelType_KEY_MATERIAL_EXPIRES
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ExpirationModelType = ExpirationModelType'
  { fromExpirationModelType ::
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

pattern ExpirationModelType_KEY_MATERIAL_DOES_NOT_EXPIRE :: ExpirationModelType
pattern ExpirationModelType_KEY_MATERIAL_DOES_NOT_EXPIRE = ExpirationModelType' "KEY_MATERIAL_DOES_NOT_EXPIRE"

pattern ExpirationModelType_KEY_MATERIAL_EXPIRES :: ExpirationModelType
pattern ExpirationModelType_KEY_MATERIAL_EXPIRES = ExpirationModelType' "KEY_MATERIAL_EXPIRES"

{-# COMPLETE
  ExpirationModelType_KEY_MATERIAL_DOES_NOT_EXPIRE,
  ExpirationModelType_KEY_MATERIAL_EXPIRES,
  ExpirationModelType'
  #-}
