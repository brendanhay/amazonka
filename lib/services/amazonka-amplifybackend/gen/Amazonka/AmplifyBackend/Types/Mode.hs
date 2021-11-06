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
-- Module      : Amazonka.AmplifyBackend.Types.Mode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.Mode
  ( Mode
      ( ..,
        Mode_AMAZON_COGNITO_USER_POOLS,
        Mode_API_KEY,
        Mode_AWS_IAM,
        Mode_OPENID_CONNECT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype Mode = Mode' {fromMode :: Core.Text}
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

pattern Mode_AMAZON_COGNITO_USER_POOLS :: Mode
pattern Mode_AMAZON_COGNITO_USER_POOLS = Mode' "AMAZON_COGNITO_USER_POOLS"

pattern Mode_API_KEY :: Mode
pattern Mode_API_KEY = Mode' "API_KEY"

pattern Mode_AWS_IAM :: Mode
pattern Mode_AWS_IAM = Mode' "AWS_IAM"

pattern Mode_OPENID_CONNECT :: Mode
pattern Mode_OPENID_CONNECT = Mode' "OPENID_CONNECT"

{-# COMPLETE
  Mode_AMAZON_COGNITO_USER_POOLS,
  Mode_API_KEY,
  Mode_AWS_IAM,
  Mode_OPENID_CONNECT,
  Mode'
  #-}
