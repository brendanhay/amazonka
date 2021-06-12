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
-- Module      : Network.AWS.CloudWatchEvents.Types.ConnectionAuthorizationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.ConnectionAuthorizationType
  ( ConnectionAuthorizationType
      ( ..,
        ConnectionAuthorizationType_API_KEY,
        ConnectionAuthorizationType_BASIC,
        ConnectionAuthorizationType_OAUTH_CLIENT_CREDENTIALS
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ConnectionAuthorizationType = ConnectionAuthorizationType'
  { fromConnectionAuthorizationType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
