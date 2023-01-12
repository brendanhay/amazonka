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
-- Module      : Amazonka.AppMesh.Types.HttpMethod
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.HttpMethod
  ( HttpMethod
      ( ..,
        HttpMethod_CONNECT,
        HttpMethod_DELETE,
        HttpMethod_GET,
        HttpMethod_HEAD,
        HttpMethod_OPTIONS,
        HttpMethod_PATCH,
        HttpMethod_POST,
        HttpMethod_PUT,
        HttpMethod_TRACE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype HttpMethod = HttpMethod'
  { fromHttpMethod ::
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

pattern HttpMethod_CONNECT :: HttpMethod
pattern HttpMethod_CONNECT = HttpMethod' "CONNECT"

pattern HttpMethod_DELETE :: HttpMethod
pattern HttpMethod_DELETE = HttpMethod' "DELETE"

pattern HttpMethod_GET :: HttpMethod
pattern HttpMethod_GET = HttpMethod' "GET"

pattern HttpMethod_HEAD :: HttpMethod
pattern HttpMethod_HEAD = HttpMethod' "HEAD"

pattern HttpMethod_OPTIONS :: HttpMethod
pattern HttpMethod_OPTIONS = HttpMethod' "OPTIONS"

pattern HttpMethod_PATCH :: HttpMethod
pattern HttpMethod_PATCH = HttpMethod' "PATCH"

pattern HttpMethod_POST :: HttpMethod
pattern HttpMethod_POST = HttpMethod' "POST"

pattern HttpMethod_PUT :: HttpMethod
pattern HttpMethod_PUT = HttpMethod' "PUT"

pattern HttpMethod_TRACE :: HttpMethod
pattern HttpMethod_TRACE = HttpMethod' "TRACE"

{-# COMPLETE
  HttpMethod_CONNECT,
  HttpMethod_DELETE,
  HttpMethod_GET,
  HttpMethod_HEAD,
  HttpMethod_OPTIONS,
  HttpMethod_PATCH,
  HttpMethod_POST,
  HttpMethod_PUT,
  HttpMethod_TRACE,
  HttpMethod'
  #-}
