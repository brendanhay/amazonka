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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UsernameAttributeType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UsernameAttributeType
  ( UsernameAttributeType
      ( ..,
        UsernameAttributeType_Email,
        UsernameAttributeType_Phone_number
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype UsernameAttributeType = UsernameAttributeType'
  { fromUsernameAttributeType ::
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

pattern UsernameAttributeType_Email :: UsernameAttributeType
pattern UsernameAttributeType_Email = UsernameAttributeType' "email"

pattern UsernameAttributeType_Phone_number :: UsernameAttributeType
pattern UsernameAttributeType_Phone_number = UsernameAttributeType' "phone_number"

{-# COMPLETE
  UsernameAttributeType_Email,
  UsernameAttributeType_Phone_number,
  UsernameAttributeType'
  #-}
