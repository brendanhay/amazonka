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
-- Module      : Network.AWS.AmplifyBackend.Types.SignInMethod
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AmplifyBackend.Types.SignInMethod
  ( SignInMethod
      ( ..,
        SignInMethod_EMAIL,
        SignInMethod_EMAIL_AND_PHONE_NUMBER,
        SignInMethod_PHONE_NUMBER,
        SignInMethod_USERNAME
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype SignInMethod = SignInMethod'
  { fromSignInMethod ::
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

pattern SignInMethod_EMAIL :: SignInMethod
pattern SignInMethod_EMAIL = SignInMethod' "EMAIL"

pattern SignInMethod_EMAIL_AND_PHONE_NUMBER :: SignInMethod
pattern SignInMethod_EMAIL_AND_PHONE_NUMBER = SignInMethod' "EMAIL_AND_PHONE_NUMBER"

pattern SignInMethod_PHONE_NUMBER :: SignInMethod
pattern SignInMethod_PHONE_NUMBER = SignInMethod' "PHONE_NUMBER"

pattern SignInMethod_USERNAME :: SignInMethod
pattern SignInMethod_USERNAME = SignInMethod' "USERNAME"

{-# COMPLETE
  SignInMethod_EMAIL,
  SignInMethod_EMAIL_AND_PHONE_NUMBER,
  SignInMethod_PHONE_NUMBER,
  SignInMethod_USERNAME,
  SignInMethod'
  #-}
