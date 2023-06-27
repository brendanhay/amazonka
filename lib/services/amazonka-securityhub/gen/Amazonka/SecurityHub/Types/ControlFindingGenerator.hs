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
-- Module      : Amazonka.SecurityHub.Types.ControlFindingGenerator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.ControlFindingGenerator
  ( ControlFindingGenerator
      ( ..,
        ControlFindingGenerator_SECURITY_CONTROL,
        ControlFindingGenerator_STANDARD_CONTROL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ControlFindingGenerator = ControlFindingGenerator'
  { fromControlFindingGenerator ::
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

pattern ControlFindingGenerator_SECURITY_CONTROL :: ControlFindingGenerator
pattern ControlFindingGenerator_SECURITY_CONTROL = ControlFindingGenerator' "SECURITY_CONTROL"

pattern ControlFindingGenerator_STANDARD_CONTROL :: ControlFindingGenerator
pattern ControlFindingGenerator_STANDARD_CONTROL = ControlFindingGenerator' "STANDARD_CONTROL"

{-# COMPLETE
  ControlFindingGenerator_SECURITY_CONTROL,
  ControlFindingGenerator_STANDARD_CONTROL,
  ControlFindingGenerator'
  #-}
