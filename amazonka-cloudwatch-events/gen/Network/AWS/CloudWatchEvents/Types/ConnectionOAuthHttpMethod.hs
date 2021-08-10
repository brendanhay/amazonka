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
-- Module      : Network.AWS.CloudWatchEvents.Types.ConnectionOAuthHttpMethod
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.ConnectionOAuthHttpMethod
  ( ConnectionOAuthHttpMethod
      ( ..,
        ConnectionOAuthHttpMethod_GET,
        ConnectionOAuthHttpMethod_POST,
        ConnectionOAuthHttpMethod_PUT
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ConnectionOAuthHttpMethod = ConnectionOAuthHttpMethod'
  { fromConnectionOAuthHttpMethod ::
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

pattern ConnectionOAuthHttpMethod_GET :: ConnectionOAuthHttpMethod
pattern ConnectionOAuthHttpMethod_GET = ConnectionOAuthHttpMethod' "GET"

pattern ConnectionOAuthHttpMethod_POST :: ConnectionOAuthHttpMethod
pattern ConnectionOAuthHttpMethod_POST = ConnectionOAuthHttpMethod' "POST"

pattern ConnectionOAuthHttpMethod_PUT :: ConnectionOAuthHttpMethod
pattern ConnectionOAuthHttpMethod_PUT = ConnectionOAuthHttpMethod' "PUT"

{-# COMPLETE
  ConnectionOAuthHttpMethod_GET,
  ConnectionOAuthHttpMethod_POST,
  ConnectionOAuthHttpMethod_PUT,
  ConnectionOAuthHttpMethod'
  #-}
