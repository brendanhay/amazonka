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
-- Module      : Amazonka.EC2.Types.BootModeType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.BootModeType
  ( BootModeType
      ( ..,
        BootModeType_Legacy_bios,
        BootModeType_Uefi
      ),
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype BootModeType = BootModeType'
  { fromBootModeType ::
      Core.Text
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

pattern BootModeType_Legacy_bios :: BootModeType
pattern BootModeType_Legacy_bios = BootModeType' "legacy-bios"

pattern BootModeType_Uefi :: BootModeType
pattern BootModeType_Uefi = BootModeType' "uefi"

{-# COMPLETE
  BootModeType_Legacy_bios,
  BootModeType_Uefi,
  BootModeType'
  #-}
