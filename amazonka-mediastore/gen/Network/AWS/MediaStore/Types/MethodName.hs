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
-- Module      : Network.AWS.MediaStore.Types.MethodName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaStore.Types.MethodName
  ( MethodName
      ( ..,
        MethodName_DELETE,
        MethodName_GET,
        MethodName_HEAD,
        MethodName_PUT
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype MethodName = MethodName'
  { fromMethodName ::
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

pattern MethodName_DELETE :: MethodName
pattern MethodName_DELETE = MethodName' "DELETE"

pattern MethodName_GET :: MethodName
pattern MethodName_GET = MethodName' "GET"

pattern MethodName_HEAD :: MethodName
pattern MethodName_HEAD = MethodName' "HEAD"

pattern MethodName_PUT :: MethodName
pattern MethodName_PUT = MethodName' "PUT"

{-# COMPLETE
  MethodName_DELETE,
  MethodName_GET,
  MethodName_HEAD,
  MethodName_PUT,
  MethodName'
  #-}
