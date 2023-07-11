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
-- Module      : Amazonka.CloudWatchEvents.Types.ConnectionAuthorizationType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.ConnectionAuthorizationType
  ( ConnectionAuthorizationType
      ( ..,
        ConnectionAuthorizationType_API_KEY,
        ConnectionAuthorizationType_BASIC,
        ConnectionAuthorizationType_OAUTH_CLIENT_CREDENTIALS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ConnectionAuthorizationType = ConnectionAuthorizationType'
  { fromConnectionAuthorizationType ::
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

pattern ConnectionAuthorizationType_API_KEY :: ConnectionAuthorizationType
pattern ConnectionAuthorizationType_API_KEY = ConnectionAuthorizationType' "API_KEY"

pattern ConnectionAuthorizationType_BASIC :: ConnectionAuthorizationType
pattern ConnectionAuthorizationType_BASIC = ConnectionAuthorizationType' "BASIC"

pattern ConnectionAuthorizationType_OAUTH_CLIENT_CREDENTIALS :: ConnectionAuthorizationType
pattern ConnectionAuthorizationType_OAUTH_CLIENT_CREDENTIALS = ConnectionAuthorizationType' "OAUTH_CLIENT_CREDENTIALS"

{-# COMPLETE
  ConnectionAuthorizationType_API_KEY,
  ConnectionAuthorizationType_BASIC,
  ConnectionAuthorizationType_OAUTH_CLIENT_CREDENTIALS,
  ConnectionAuthorizationType'
  #-}
