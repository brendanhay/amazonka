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

import qualified Network.AWS.Prelude as Prelude

newtype EmailSendingAccountType = EmailSendingAccountType'
  { fromEmailSendingAccountType ::
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

pattern EmailSendingAccountType_COGNITO_DEFAULT :: EmailSendingAccountType
pattern EmailSendingAccountType_COGNITO_DEFAULT = EmailSendingAccountType' "COGNITO_DEFAULT"

pattern EmailSendingAccountType_DEVELOPER :: EmailSendingAccountType
pattern EmailSendingAccountType_DEVELOPER = EmailSendingAccountType' "DEVELOPER"

{-# COMPLETE
  EmailSendingAccountType_COGNITO_DEFAULT,
  EmailSendingAccountType_DEVELOPER,
  EmailSendingAccountType'
  #-}
