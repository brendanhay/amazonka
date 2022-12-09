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
-- Module      : Amazonka.GameLift.Types.ComputeStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.ComputeStatus
  ( ComputeStatus
      ( ..,
        ComputeStatus_ACTIVE,
        ComputeStatus_PENDING,
        ComputeStatus_TERMINATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ComputeStatus = ComputeStatus'
  { fromComputeStatus ::
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

pattern ComputeStatus_ACTIVE :: ComputeStatus
pattern ComputeStatus_ACTIVE = ComputeStatus' "ACTIVE"

pattern ComputeStatus_PENDING :: ComputeStatus
pattern ComputeStatus_PENDING = ComputeStatus' "PENDING"

pattern ComputeStatus_TERMINATING :: ComputeStatus
pattern ComputeStatus_TERMINATING = ComputeStatus' "TERMINATING"

{-# COMPLETE
  ComputeStatus_ACTIVE,
  ComputeStatus_PENDING,
  ComputeStatus_TERMINATING,
  ComputeStatus'
  #-}
