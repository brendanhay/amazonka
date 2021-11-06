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
-- Module      : Amazonka.MwAA.Types.UpdateStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MwAA.Types.UpdateStatus
  ( UpdateStatus
      ( ..,
        UpdateStatus_FAILED,
        UpdateStatus_PENDING,
        UpdateStatus_SUCCESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype UpdateStatus = UpdateStatus'
  { fromUpdateStatus ::
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

pattern UpdateStatus_FAILED :: UpdateStatus
pattern UpdateStatus_FAILED = UpdateStatus' "FAILED"

pattern UpdateStatus_PENDING :: UpdateStatus
pattern UpdateStatus_PENDING = UpdateStatus' "PENDING"

pattern UpdateStatus_SUCCESS :: UpdateStatus
pattern UpdateStatus_SUCCESS = UpdateStatus' "SUCCESS"

{-# COMPLETE
  UpdateStatus_FAILED,
  UpdateStatus_PENDING,
  UpdateStatus_SUCCESS,
  UpdateStatus'
  #-}
