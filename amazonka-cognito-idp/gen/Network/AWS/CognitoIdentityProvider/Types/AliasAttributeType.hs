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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AliasAttributeType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AliasAttributeType
  ( AliasAttributeType
      ( ..,
        AliasAttributeType_Email,
        AliasAttributeType_Phone_number,
        AliasAttributeType_Preferred_username
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype AliasAttributeType = AliasAttributeType'
  { fromAliasAttributeType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
