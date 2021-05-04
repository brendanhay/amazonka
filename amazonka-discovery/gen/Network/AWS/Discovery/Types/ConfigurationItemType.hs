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
-- Module      : Network.AWS.Discovery.Types.ConfigurationItemType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.ConfigurationItemType
  ( ConfigurationItemType
      ( ..,
        ConfigurationItemType_APPLICATION,
        ConfigurationItemType_CONNECTION,
        ConfigurationItemType_PROCESS,
        ConfigurationItemType_SERVER
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ConfigurationItemType = ConfigurationItemType'
  { fromConfigurationItemType ::
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

pattern ConfigurationItemType_APPLICATION :: ConfigurationItemType
pattern ConfigurationItemType_APPLICATION = ConfigurationItemType' "APPLICATION"

pattern ConfigurationItemType_CONNECTION :: ConfigurationItemType
pattern ConfigurationItemType_CONNECTION = ConfigurationItemType' "CONNECTION"

pattern ConfigurationItemType_PROCESS :: ConfigurationItemType
pattern ConfigurationItemType_PROCESS = ConfigurationItemType' "PROCESS"

pattern ConfigurationItemType_SERVER :: ConfigurationItemType
pattern ConfigurationItemType_SERVER = ConfigurationItemType' "SERVER"

{-# COMPLETE
  ConfigurationItemType_APPLICATION,
  ConfigurationItemType_CONNECTION,
  ConfigurationItemType_PROCESS,
  ConfigurationItemType_SERVER,
  ConfigurationItemType'
  #-}
