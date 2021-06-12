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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.EmailSendingAccountType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.EmailSendingAccountType
  ( EmailSendingAccountType
      ( ..,
        EmailSendingAccountType_COGNITO_DEFAULT,
        EmailSendingAccountType_DEVELOPER
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype EmailSendingAccountType = EmailSendingAccountType'
  { fromEmailSendingAccountType ::
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

pattern EmailSendingAccountType_COGNITO_DEFAULT :: EmailSendingAccountType
pattern EmailSendingAccountType_COGNITO_DEFAULT = EmailSendingAccountType' "COGNITO_DEFAULT"

pattern EmailSendingAccountType_DEVELOPER :: EmailSendingAccountType
pattern EmailSendingAccountType_DEVELOPER = EmailSendingAccountType' "DEVELOPER"

{-# COMPLETE
  EmailSendingAccountType_COGNITO_DEFAULT,
  EmailSendingAccountType_DEVELOPER,
  EmailSendingAccountType'
  #-}
