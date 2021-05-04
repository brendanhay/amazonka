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

import qualified Network.AWS.Prelude as Prelude

newtype AliasAttributeType = AliasAttributeType'
  { fromAliasAttributeType ::
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
