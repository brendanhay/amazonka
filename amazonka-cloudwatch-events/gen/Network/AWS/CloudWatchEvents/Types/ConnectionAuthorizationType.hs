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

import qualified Network.AWS.Prelude as Prelude

newtype ConnectionAuthorizationType = ConnectionAuthorizationType'
  { fromConnectionAuthorizationType ::
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
