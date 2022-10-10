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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
import qualified Amazonka.Prelude as Prelude

newtype AliasAttributeType = AliasAttributeType'
  { fromAliasAttributeType ::
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
