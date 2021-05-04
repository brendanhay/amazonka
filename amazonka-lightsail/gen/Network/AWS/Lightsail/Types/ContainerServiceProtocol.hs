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
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceProtocol
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerServiceProtocol
  ( ContainerServiceProtocol
      ( ..,
        ContainerServiceProtocol_HTTP,
        ContainerServiceProtocol_HTTPS,
        ContainerServiceProtocol_TCP,
        ContainerServiceProtocol_UDP
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ContainerServiceProtocol = ContainerServiceProtocol'
  { fromContainerServiceProtocol ::
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

pattern ContainerServiceProtocol_HTTP :: ContainerServiceProtocol
pattern ContainerServiceProtocol_HTTP = ContainerServiceProtocol' "HTTP"

pattern ContainerServiceProtocol_HTTPS :: ContainerServiceProtocol
pattern ContainerServiceProtocol_HTTPS = ContainerServiceProtocol' "HTTPS"

pattern ContainerServiceProtocol_TCP :: ContainerServiceProtocol
pattern ContainerServiceProtocol_TCP = ContainerServiceProtocol' "TCP"

pattern ContainerServiceProtocol_UDP :: ContainerServiceProtocol
pattern ContainerServiceProtocol_UDP = ContainerServiceProtocol' "UDP"

{-# COMPLETE
  ContainerServiceProtocol_HTTP,
  ContainerServiceProtocol_HTTPS,
  ContainerServiceProtocol_TCP,
  ContainerServiceProtocol_UDP,
  ContainerServiceProtocol'
  #-}
