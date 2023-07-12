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
-- Module      : Amazonka.ElastiCache.Types.InputAuthenticationType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.InputAuthenticationType
  ( InputAuthenticationType
      ( ..,
        InputAuthenticationType_Iam,
        InputAuthenticationType_No_password_required,
        InputAuthenticationType_Password
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InputAuthenticationType = InputAuthenticationType'
  { fromInputAuthenticationType ::
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

pattern InputAuthenticationType_Iam :: InputAuthenticationType
pattern InputAuthenticationType_Iam = InputAuthenticationType' "iam"

pattern InputAuthenticationType_No_password_required :: InputAuthenticationType
pattern InputAuthenticationType_No_password_required = InputAuthenticationType' "no-password-required"

pattern InputAuthenticationType_Password :: InputAuthenticationType
pattern InputAuthenticationType_Password = InputAuthenticationType' "password"

{-# COMPLETE
  InputAuthenticationType_Iam,
  InputAuthenticationType_No_password_required,
  InputAuthenticationType_Password,
  InputAuthenticationType'
  #-}
