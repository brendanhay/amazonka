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
-- Module      : Amazonka.CognitoIdentityProvider.Types.AliasAttributeType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.AliasAttributeType
  ( AliasAttributeType
      ( ..,
        AliasAttributeType_Email,
        AliasAttributeType_Phone_number,
        AliasAttributeType_Preferred_username
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AliasAttributeType = AliasAttributeType'
  { fromAliasAttributeType ::
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

pattern AliasAttributeType_Email :: AliasAttributeType
pattern AliasAttributeType_Email = AliasAttributeType' "email"

pattern AliasAttributeType_Phone_number :: AliasAttributeType
pattern AliasAttributeType_Phone_number = AliasAttributeType' "phone_number"

pattern AliasAttributeType_Preferred_username :: AliasAttributeType
pattern AliasAttributeType_Preferred_username = AliasAttributeType' "preferred_username"

{-# COMPLETE
  AliasAttributeType_Email,
  AliasAttributeType_Phone_number,
  AliasAttributeType_Preferred_username,
  AliasAttributeType'
  #-}
