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
-- Module      : Amazonka.AmplifyBackend.Types.SignInMethod
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.SignInMethod
  ( SignInMethod
      ( ..,
        SignInMethod_EMAIL,
        SignInMethod_EMAIL_AND_PHONE_NUMBER,
        SignInMethod_PHONE_NUMBER,
        SignInMethod_USERNAME
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SignInMethod = SignInMethod'
  { fromSignInMethod ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
