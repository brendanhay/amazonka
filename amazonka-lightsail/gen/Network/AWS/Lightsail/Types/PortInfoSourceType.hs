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
-- Module      : Network.AWS.Lightsail.Types.PortInfoSourceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.PortInfoSourceType
  ( PortInfoSourceType
      ( ..,
        PortInfoSourceType_CLOSED,
        PortInfoSourceType_DEFAULT,
        PortInfoSourceType_INSTANCE,
        PortInfoSourceType_NONE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype PortInfoSourceType = PortInfoSourceType'
  { fromPortInfoSourceType ::
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
