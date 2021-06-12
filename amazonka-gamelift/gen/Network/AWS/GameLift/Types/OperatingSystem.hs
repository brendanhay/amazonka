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
-- Module      : Network.AWS.GameLift.Types.OperatingSystem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.OperatingSystem
  ( OperatingSystem
      ( ..,
        OperatingSystem_AMAZON_LINUX,
        OperatingSystem_AMAZON_LINUX_2,
        OperatingSystem_WINDOWS_2012
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype OperatingSystem = OperatingSystem'
  { fromOperatingSystem ::
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

pattern OperatingSystem_AMAZON_LINUX :: OperatingSystem
pattern OperatingSystem_AMAZON_LINUX = OperatingSystem' "AMAZON_LINUX"

pattern OperatingSystem_AMAZON_LINUX_2 :: OperatingSystem
pattern OperatingSystem_AMAZON_LINUX_2 = OperatingSystem' "AMAZON_LINUX_2"

pattern OperatingSystem_WINDOWS_2012 :: OperatingSystem
pattern OperatingSystem_WINDOWS_2012 = OperatingSystem' "WINDOWS_2012"

{-# COMPLETE
  OperatingSystem_AMAZON_LINUX,
  OperatingSystem_AMAZON_LINUX_2,
  OperatingSystem_WINDOWS_2012,
  OperatingSystem'
  #-}
