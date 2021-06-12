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

import qualified Network.AWS.Core as Core

newtype CompromisedCredentialsEventActionType = CompromisedCredentialsEventActionType'
  { fromCompromisedCredentialsEventActionType ::
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

pattern CompromisedCredentialsEventActionType_BLOCK :: CompromisedCredentialsEventActionType
pattern CompromisedCredentialsEventActionType_BLOCK = CompromisedCredentialsEventActionType' "BLOCK"

pattern CompromisedCredentialsEventActionType_NO_ACTION :: CompromisedCredentialsEventActionType
pattern CompromisedCredentialsEventActionType_NO_ACTION = CompromisedCredentialsEventActionType' "NO_ACTION"

{-# COMPLETE
  CompromisedCredentialsEventActionType_BLOCK,
  CompromisedCredentialsEventActionType_NO_ACTION,
  CompromisedCredentialsEventActionType'
  #-}
