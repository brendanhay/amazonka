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
-- Module      : Amazonka.M2.Types.EnvironmentLifecycle
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.EnvironmentLifecycle
  ( EnvironmentLifecycle
      ( ..,
        EnvironmentLifecycle_Available,
        EnvironmentLifecycle_Creating,
        EnvironmentLifecycle_Deleting,
        EnvironmentLifecycle_Failed
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EnvironmentLifecycle = EnvironmentLifecycle'
  { fromEnvironmentLifecycle ::
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

pattern EnvironmentLifecycle_Available :: EnvironmentLifecycle
pattern EnvironmentLifecycle_Available = EnvironmentLifecycle' "Available"

pattern EnvironmentLifecycle_Creating :: EnvironmentLifecycle
pattern EnvironmentLifecycle_Creating = EnvironmentLifecycle' "Creating"

pattern EnvironmentLifecycle_Deleting :: EnvironmentLifecycle
pattern EnvironmentLifecycle_Deleting = EnvironmentLifecycle' "Deleting"

pattern EnvironmentLifecycle_Failed :: EnvironmentLifecycle
pattern EnvironmentLifecycle_Failed = EnvironmentLifecycle' "Failed"

{-# COMPLETE
  EnvironmentLifecycle_Available,
  EnvironmentLifecycle_Creating,
  EnvironmentLifecycle_Deleting,
  EnvironmentLifecycle_Failed,
  EnvironmentLifecycle'
  #-}
