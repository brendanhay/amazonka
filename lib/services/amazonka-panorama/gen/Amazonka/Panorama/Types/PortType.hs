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
-- Module      : Amazonka.Panorama.Types.PortType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.PortType
  ( PortType
      ( ..,
        PortType_BOOLEAN,
        PortType_FLOAT32,
        PortType_INT32,
        PortType_MEDIA,
        PortType_STRING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PortType = PortType'
  { fromPortType ::
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

pattern PortType_BOOLEAN :: PortType
pattern PortType_BOOLEAN = PortType' "BOOLEAN"

pattern PortType_FLOAT32 :: PortType
pattern PortType_FLOAT32 = PortType' "FLOAT32"

pattern PortType_INT32 :: PortType
pattern PortType_INT32 = PortType' "INT32"

pattern PortType_MEDIA :: PortType
pattern PortType_MEDIA = PortType' "MEDIA"

pattern PortType_STRING :: PortType
pattern PortType_STRING = PortType' "STRING"

{-# COMPLETE
  PortType_BOOLEAN,
  PortType_FLOAT32,
  PortType_INT32,
  PortType_MEDIA,
  PortType_STRING,
  PortType'
  #-}
