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
-- Module      : Amazonka.QuickSight.Types.AuthenticationMethodOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AuthenticationMethodOption
  ( AuthenticationMethodOption
      ( ..,
        AuthenticationMethodOption_ACTIVE_DIRECTORY,
        AuthenticationMethodOption_IAM_AND_QUICKSIGHT,
        AuthenticationMethodOption_IAM_ONLY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AuthenticationMethodOption = AuthenticationMethodOption'
  { fromAuthenticationMethodOption ::
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

pattern AuthenticationMethodOption_ACTIVE_DIRECTORY :: AuthenticationMethodOption
pattern AuthenticationMethodOption_ACTIVE_DIRECTORY = AuthenticationMethodOption' "ACTIVE_DIRECTORY"

pattern AuthenticationMethodOption_IAM_AND_QUICKSIGHT :: AuthenticationMethodOption
pattern AuthenticationMethodOption_IAM_AND_QUICKSIGHT = AuthenticationMethodOption' "IAM_AND_QUICKSIGHT"

pattern AuthenticationMethodOption_IAM_ONLY :: AuthenticationMethodOption
pattern AuthenticationMethodOption_IAM_ONLY = AuthenticationMethodOption' "IAM_ONLY"

{-# COMPLETE
  AuthenticationMethodOption_ACTIVE_DIRECTORY,
  AuthenticationMethodOption_IAM_AND_QUICKSIGHT,
  AuthenticationMethodOption_IAM_ONLY,
  AuthenticationMethodOption'
  #-}
