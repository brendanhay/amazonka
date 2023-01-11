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
-- Module      : Amazonka.AppFlow.Types.OAuth2CustomPropType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.OAuth2CustomPropType
  ( OAuth2CustomPropType
      ( ..,
        OAuth2CustomPropType_AUTH_URL,
        OAuth2CustomPropType_TOKEN_URL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OAuth2CustomPropType = OAuth2CustomPropType'
  { fromOAuth2CustomPropType ::
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

pattern OAuth2CustomPropType_AUTH_URL :: OAuth2CustomPropType
pattern OAuth2CustomPropType_AUTH_URL = OAuth2CustomPropType' "AUTH_URL"

pattern OAuth2CustomPropType_TOKEN_URL :: OAuth2CustomPropType
pattern OAuth2CustomPropType_TOKEN_URL = OAuth2CustomPropType' "TOKEN_URL"

{-# COMPLETE
  OAuth2CustomPropType_AUTH_URL,
  OAuth2CustomPropType_TOKEN_URL,
  OAuth2CustomPropType'
  #-}
