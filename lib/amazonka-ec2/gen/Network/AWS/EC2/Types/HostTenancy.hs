{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.HostTenancy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.HostTenancy where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data HostTenancy
  = HTDedicated
  | HTHost
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText HostTenancy where
  parser =
    takeLowerText >>= \case
      "dedicated" -> pure HTDedicated
      "host" -> pure HTHost
      e ->
        fromTextError $
          "Failure parsing HostTenancy from value: '" <> e
            <> "'. Accepted values: dedicated, host"

instance ToText HostTenancy where
  toText = \case
    HTDedicated -> "dedicated"
    HTHost -> "host"

instance Hashable HostTenancy

instance NFData HostTenancy

instance ToByteString HostTenancy

instance ToQuery HostTenancy

instance ToHeader HostTenancy
