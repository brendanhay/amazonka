{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.EmailSendingAccountType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.EmailSendingAccountType
  ( EmailSendingAccountType
      ( EmailSendingAccountType',
        EmailSendingAccountTypeCognitoDefault,
        EmailSendingAccountTypeDeveloper,
        fromEmailSendingAccountType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype EmailSendingAccountType = EmailSendingAccountType'
  { fromEmailSendingAccountType ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern EmailSendingAccountTypeCognitoDefault :: EmailSendingAccountType
pattern EmailSendingAccountTypeCognitoDefault = EmailSendingAccountType' "COGNITO_DEFAULT"

pattern EmailSendingAccountTypeDeveloper :: EmailSendingAccountType
pattern EmailSendingAccountTypeDeveloper = EmailSendingAccountType' "DEVELOPER"

{-# COMPLETE
  EmailSendingAccountTypeCognitoDefault,
  EmailSendingAccountTypeDeveloper,
  EmailSendingAccountType'
  #-}
