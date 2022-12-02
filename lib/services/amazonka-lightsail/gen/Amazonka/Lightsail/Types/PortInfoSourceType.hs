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
-- Module      : Amazonka.Lightsail.Types.PortInfoSourceType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.PortInfoSourceType
  ( PortInfoSourceType
      ( ..,
        PortInfoSourceType_CLOSED,
        PortInfoSourceType_DEFAULT,
        PortInfoSourceType_INSTANCE,
        PortInfoSourceType_NONE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PortInfoSourceType = PortInfoSourceType'
  { fromPortInfoSourceType ::
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

pattern PortInfoSourceType_CLOSED :: PortInfoSourceType
pattern PortInfoSourceType_CLOSED = PortInfoSourceType' "CLOSED"

pattern PortInfoSourceType_DEFAULT :: PortInfoSourceType
pattern PortInfoSourceType_DEFAULT = PortInfoSourceType' "DEFAULT"

pattern PortInfoSourceType_INSTANCE :: PortInfoSourceType
pattern PortInfoSourceType_INSTANCE = PortInfoSourceType' "INSTANCE"

pattern PortInfoSourceType_NONE :: PortInfoSourceType
pattern PortInfoSourceType_NONE = PortInfoSourceType' "NONE"

{-# COMPLETE
  PortInfoSourceType_CLOSED,
  PortInfoSourceType_DEFAULT,
  PortInfoSourceType_INSTANCE,
  PortInfoSourceType_NONE,
  PortInfoSourceType'
  #-}
