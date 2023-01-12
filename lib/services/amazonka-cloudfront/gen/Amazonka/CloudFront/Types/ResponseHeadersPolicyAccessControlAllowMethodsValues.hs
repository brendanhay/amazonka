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
-- Module      : Amazonka.CloudFront.Types.ResponseHeadersPolicyAccessControlAllowMethodsValues
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.ResponseHeadersPolicyAccessControlAllowMethodsValues
  ( ResponseHeadersPolicyAccessControlAllowMethodsValues
      ( ..,
        ResponseHeadersPolicyAccessControlAllowMethodsValues_ALL,
        ResponseHeadersPolicyAccessControlAllowMethodsValues_DELETE,
        ResponseHeadersPolicyAccessControlAllowMethodsValues_GET,
        ResponseHeadersPolicyAccessControlAllowMethodsValues_HEAD,
        ResponseHeadersPolicyAccessControlAllowMethodsValues_OPTIONS,
        ResponseHeadersPolicyAccessControlAllowMethodsValues_PATCH,
        ResponseHeadersPolicyAccessControlAllowMethodsValues_POST,
        ResponseHeadersPolicyAccessControlAllowMethodsValues_PUT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResponseHeadersPolicyAccessControlAllowMethodsValues = ResponseHeadersPolicyAccessControlAllowMethodsValues'
  { fromResponseHeadersPolicyAccessControlAllowMethodsValues ::
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

pattern ResponseHeadersPolicyAccessControlAllowMethodsValues_ALL :: ResponseHeadersPolicyAccessControlAllowMethodsValues
pattern ResponseHeadersPolicyAccessControlAllowMethodsValues_ALL = ResponseHeadersPolicyAccessControlAllowMethodsValues' "ALL"

pattern ResponseHeadersPolicyAccessControlAllowMethodsValues_DELETE :: ResponseHeadersPolicyAccessControlAllowMethodsValues
pattern ResponseHeadersPolicyAccessControlAllowMethodsValues_DELETE = ResponseHeadersPolicyAccessControlAllowMethodsValues' "DELETE"

pattern ResponseHeadersPolicyAccessControlAllowMethodsValues_GET :: ResponseHeadersPolicyAccessControlAllowMethodsValues
pattern ResponseHeadersPolicyAccessControlAllowMethodsValues_GET = ResponseHeadersPolicyAccessControlAllowMethodsValues' "GET"

pattern ResponseHeadersPolicyAccessControlAllowMethodsValues_HEAD :: ResponseHeadersPolicyAccessControlAllowMethodsValues
pattern ResponseHeadersPolicyAccessControlAllowMethodsValues_HEAD = ResponseHeadersPolicyAccessControlAllowMethodsValues' "HEAD"

pattern ResponseHeadersPolicyAccessControlAllowMethodsValues_OPTIONS :: ResponseHeadersPolicyAccessControlAllowMethodsValues
pattern ResponseHeadersPolicyAccessControlAllowMethodsValues_OPTIONS = ResponseHeadersPolicyAccessControlAllowMethodsValues' "OPTIONS"

pattern ResponseHeadersPolicyAccessControlAllowMethodsValues_PATCH :: ResponseHeadersPolicyAccessControlAllowMethodsValues
pattern ResponseHeadersPolicyAccessControlAllowMethodsValues_PATCH = ResponseHeadersPolicyAccessControlAllowMethodsValues' "PATCH"

pattern ResponseHeadersPolicyAccessControlAllowMethodsValues_POST :: ResponseHeadersPolicyAccessControlAllowMethodsValues
pattern ResponseHeadersPolicyAccessControlAllowMethodsValues_POST = ResponseHeadersPolicyAccessControlAllowMethodsValues' "POST"

pattern ResponseHeadersPolicyAccessControlAllowMethodsValues_PUT :: ResponseHeadersPolicyAccessControlAllowMethodsValues
pattern ResponseHeadersPolicyAccessControlAllowMethodsValues_PUT = ResponseHeadersPolicyAccessControlAllowMethodsValues' "PUT"

{-# COMPLETE
  ResponseHeadersPolicyAccessControlAllowMethodsValues_ALL,
  ResponseHeadersPolicyAccessControlAllowMethodsValues_DELETE,
  ResponseHeadersPolicyAccessControlAllowMethodsValues_GET,
  ResponseHeadersPolicyAccessControlAllowMethodsValues_HEAD,
  ResponseHeadersPolicyAccessControlAllowMethodsValues_OPTIONS,
  ResponseHeadersPolicyAccessControlAllowMethodsValues_PATCH,
  ResponseHeadersPolicyAccessControlAllowMethodsValues_POST,
  ResponseHeadersPolicyAccessControlAllowMethodsValues_PUT,
  ResponseHeadersPolicyAccessControlAllowMethodsValues'
  #-}
