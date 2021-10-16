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
-- Module      : Network.AWS.CloudFront.Types.Method
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Method
  ( Method
      ( ..,
        Method_DELETE,
        Method_GET,
        Method_HEAD,
        Method_OPTIONS,
        Method_PATCH,
        Method_POST,
        Method_PUT
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype Method = Method' {fromMethod :: Core.Text}
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

pattern Method_DELETE :: Method
pattern Method_DELETE = Method' "DELETE"

pattern Method_GET :: Method
pattern Method_GET = Method' "GET"

pattern Method_HEAD :: Method
pattern Method_HEAD = Method' "HEAD"

pattern Method_OPTIONS :: Method
pattern Method_OPTIONS = Method' "OPTIONS"

pattern Method_PATCH :: Method
pattern Method_PATCH = Method' "PATCH"

pattern Method_POST :: Method
pattern Method_POST = Method' "POST"

pattern Method_PUT :: Method
pattern Method_PUT = Method' "PUT"

{-# COMPLETE
  Method_DELETE,
  Method_GET,
  Method_HEAD,
  Method_OPTIONS,
  Method_PATCH,
  Method_POST,
  Method_PUT,
  Method'
  #-}
