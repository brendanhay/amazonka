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
-- Module      : Network.AWS.Route53AutoNaming.Types.NamespaceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.NamespaceType
  ( NamespaceType
      ( ..,
        NamespaceType_DNS_PRIVATE,
        NamespaceType_DNS_PUBLIC,
        NamespaceType_HTTP
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype NamespaceType = NamespaceType'
  { fromNamespaceType ::
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

pattern NamespaceType_DNS_PRIVATE :: NamespaceType
pattern NamespaceType_DNS_PRIVATE = NamespaceType' "DNS_PRIVATE"

pattern NamespaceType_DNS_PUBLIC :: NamespaceType
pattern NamespaceType_DNS_PUBLIC = NamespaceType' "DNS_PUBLIC"

pattern NamespaceType_HTTP :: NamespaceType
pattern NamespaceType_HTTP = NamespaceType' "HTTP"

{-# COMPLETE
  NamespaceType_DNS_PRIVATE,
  NamespaceType_DNS_PUBLIC,
  NamespaceType_HTTP,
  NamespaceType'
  #-}
