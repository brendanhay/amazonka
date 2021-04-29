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
-- Module      : Network.AWS.IoT.Types.ThingIndexingMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingIndexingMode
  ( ThingIndexingMode
      ( ..,
        ThingIndexingMode_OFF,
        ThingIndexingMode_REGISTRY,
        ThingIndexingMode_REGISTRY_AND_SHADOW
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ThingIndexingMode = ThingIndexingMode'
  { fromThingIndexingMode ::
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

pattern ThingIndexingMode_OFF :: ThingIndexingMode
pattern ThingIndexingMode_OFF = ThingIndexingMode' "OFF"

pattern ThingIndexingMode_REGISTRY :: ThingIndexingMode
pattern ThingIndexingMode_REGISTRY = ThingIndexingMode' "REGISTRY"

pattern ThingIndexingMode_REGISTRY_AND_SHADOW :: ThingIndexingMode
pattern ThingIndexingMode_REGISTRY_AND_SHADOW = ThingIndexingMode' "REGISTRY_AND_SHADOW"

{-# COMPLETE
  ThingIndexingMode_OFF,
  ThingIndexingMode_REGISTRY,
  ThingIndexingMode_REGISTRY_AND_SHADOW,
  ThingIndexingMode'
  #-}
