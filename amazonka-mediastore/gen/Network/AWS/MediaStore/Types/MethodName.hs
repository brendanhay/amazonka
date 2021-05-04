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

import qualified Network.AWS.Prelude as Prelude

newtype MethodName = MethodName'
  { fromMethodName ::
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
