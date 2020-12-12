{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.EventFilterType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.EventFilterType
  ( EventFilterType
      ( EventFilterType',
        PasswordChange,
        SignIn,
        SignUp
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype EventFilterType = EventFilterType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern PasswordChange :: EventFilterType
pattern PasswordChange = EventFilterType' "PASSWORD_CHANGE"

pattern SignIn :: EventFilterType
pattern SignIn = EventFilterType' "SIGN_IN"

pattern SignUp :: EventFilterType
pattern SignUp = EventFilterType' "SIGN_UP"

{-# COMPLETE
  PasswordChange,
  SignIn,
  SignUp,
  EventFilterType'
  #-}
