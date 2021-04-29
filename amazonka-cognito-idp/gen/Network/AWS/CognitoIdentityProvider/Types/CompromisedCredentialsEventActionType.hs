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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsEventActionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsEventActionType
  ( CompromisedCredentialsEventActionType
      ( ..,
        CompromisedCredentialsEventActionType_BLOCK,
        CompromisedCredentialsEventActionType_NO_ACTION
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype CompromisedCredentialsEventActionType = CompromisedCredentialsEventActionType'
  { fromCompromisedCredentialsEventActionType ::
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

pattern CompromisedCredentialsEventActionType_BLOCK :: CompromisedCredentialsEventActionType
pattern CompromisedCredentialsEventActionType_BLOCK = CompromisedCredentialsEventActionType' "BLOCK"

pattern CompromisedCredentialsEventActionType_NO_ACTION :: CompromisedCredentialsEventActionType
pattern CompromisedCredentialsEventActionType_NO_ACTION = CompromisedCredentialsEventActionType' "NO_ACTION"

{-# COMPLETE
  CompromisedCredentialsEventActionType_BLOCK,
  CompromisedCredentialsEventActionType_NO_ACTION,
  CompromisedCredentialsEventActionType'
  #-}
