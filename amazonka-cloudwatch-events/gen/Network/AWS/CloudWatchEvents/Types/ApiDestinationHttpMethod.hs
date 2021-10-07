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
-- Module      : Network.AWS.CloudWatchEvents.Types.ApiDestinationHttpMethod
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.ApiDestinationHttpMethod
  ( ApiDestinationHttpMethod
      ( ..,
        ApiDestinationHttpMethod_DELETE,
        ApiDestinationHttpMethod_GET,
        ApiDestinationHttpMethod_HEAD,
        ApiDestinationHttpMethod_OPTIONS,
        ApiDestinationHttpMethod_PATCH,
        ApiDestinationHttpMethod_POST,
        ApiDestinationHttpMethod_PUT
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ApiDestinationHttpMethod = ApiDestinationHttpMethod'
  { fromApiDestinationHttpMethod ::
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

pattern ApiDestinationHttpMethod_DELETE :: ApiDestinationHttpMethod
pattern ApiDestinationHttpMethod_DELETE = ApiDestinationHttpMethod' "DELETE"

pattern ApiDestinationHttpMethod_GET :: ApiDestinationHttpMethod
pattern ApiDestinationHttpMethod_GET = ApiDestinationHttpMethod' "GET"

pattern ApiDestinationHttpMethod_HEAD :: ApiDestinationHttpMethod
pattern ApiDestinationHttpMethod_HEAD = ApiDestinationHttpMethod' "HEAD"

pattern ApiDestinationHttpMethod_OPTIONS :: ApiDestinationHttpMethod
pattern ApiDestinationHttpMethod_OPTIONS = ApiDestinationHttpMethod' "OPTIONS"

pattern ApiDestinationHttpMethod_PATCH :: ApiDestinationHttpMethod
pattern ApiDestinationHttpMethod_PATCH = ApiDestinationHttpMethod' "PATCH"

pattern ApiDestinationHttpMethod_POST :: ApiDestinationHttpMethod
pattern ApiDestinationHttpMethod_POST = ApiDestinationHttpMethod' "POST"

pattern ApiDestinationHttpMethod_PUT :: ApiDestinationHttpMethod
pattern ApiDestinationHttpMethod_PUT = ApiDestinationHttpMethod' "PUT"

{-# COMPLETE
  ApiDestinationHttpMethod_DELETE,
  ApiDestinationHttpMethod_GET,
  ApiDestinationHttpMethod_HEAD,
  ApiDestinationHttpMethod_OPTIONS,
  ApiDestinationHttpMethod_PATCH,
  ApiDestinationHttpMethod_POST,
  ApiDestinationHttpMethod_PUT,
  ApiDestinationHttpMethod'
  #-}
