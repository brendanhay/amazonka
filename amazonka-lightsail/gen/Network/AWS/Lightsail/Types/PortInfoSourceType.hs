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

import qualified Network.AWS.Prelude as Prelude

newtype PortInfoSourceType = PortInfoSourceType'
  { fromPortInfoSourceType ::
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
