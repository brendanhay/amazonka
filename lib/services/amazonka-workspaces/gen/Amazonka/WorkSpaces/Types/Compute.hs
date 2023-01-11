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
-- Module      : Amazonka.WorkSpaces.Types.Compute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.Compute
  ( Compute
      ( ..,
        Compute_GRAPHICS,
        Compute_GRAPHICSPRO,
        Compute_GRAPHICSPRO_G4DN,
        Compute_GRAPHICS_G4DN,
        Compute_PERFORMANCE,
        Compute_POWER,
        Compute_POWERPRO,
        Compute_STANDARD,
        Compute_VALUE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Compute = Compute' {fromCompute :: Data.Text}
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

pattern Compute_GRAPHICS :: Compute
pattern Compute_GRAPHICS = Compute' "GRAPHICS"

pattern Compute_GRAPHICSPRO :: Compute
pattern Compute_GRAPHICSPRO = Compute' "GRAPHICSPRO"

pattern Compute_GRAPHICSPRO_G4DN :: Compute
pattern Compute_GRAPHICSPRO_G4DN = Compute' "GRAPHICSPRO_G4DN"

pattern Compute_GRAPHICS_G4DN :: Compute
pattern Compute_GRAPHICS_G4DN = Compute' "GRAPHICS_G4DN"

pattern Compute_PERFORMANCE :: Compute
pattern Compute_PERFORMANCE = Compute' "PERFORMANCE"

pattern Compute_POWER :: Compute
pattern Compute_POWER = Compute' "POWER"

pattern Compute_POWERPRO :: Compute
pattern Compute_POWERPRO = Compute' "POWERPRO"

pattern Compute_STANDARD :: Compute
pattern Compute_STANDARD = Compute' "STANDARD"

pattern Compute_VALUE :: Compute
pattern Compute_VALUE = Compute' "VALUE"

{-# COMPLETE
  Compute_GRAPHICS,
  Compute_GRAPHICSPRO,
  Compute_GRAPHICSPRO_G4DN,
  Compute_GRAPHICS_G4DN,
  Compute_PERFORMANCE,
  Compute_POWER,
  Compute_POWERPRO,
  Compute_STANDARD,
  Compute_VALUE,
  Compute'
  #-}
