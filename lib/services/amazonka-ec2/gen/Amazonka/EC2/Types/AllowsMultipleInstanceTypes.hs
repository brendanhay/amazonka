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
-- Module      : Amazonka.EC2.Types.AllowsMultipleInstanceTypes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AllowsMultipleInstanceTypes
  ( AllowsMultipleInstanceTypes
      ( ..,
        AllowsMultipleInstanceTypes_Off,
        AllowsMultipleInstanceTypes_On
      ),
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype AllowsMultipleInstanceTypes = AllowsMultipleInstanceTypes'
  { fromAllowsMultipleInstanceTypes ::
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

pattern AllowsMultipleInstanceTypes_Off :: AllowsMultipleInstanceTypes
pattern AllowsMultipleInstanceTypes_Off = AllowsMultipleInstanceTypes' "off"

pattern AllowsMultipleInstanceTypes_On :: AllowsMultipleInstanceTypes
pattern AllowsMultipleInstanceTypes_On = AllowsMultipleInstanceTypes' "on"

{-# COMPLETE
  AllowsMultipleInstanceTypes_Off,
  AllowsMultipleInstanceTypes_On,
  AllowsMultipleInstanceTypes'
  #-}
