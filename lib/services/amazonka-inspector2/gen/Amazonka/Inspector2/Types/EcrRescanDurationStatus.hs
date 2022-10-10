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
-- Module      : Amazonka.Inspector2.Types.EcrRescanDurationStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.EcrRescanDurationStatus
  ( EcrRescanDurationStatus
      ( ..,
        EcrRescanDurationStatus_FAILED,
        EcrRescanDurationStatus_PENDING,
        EcrRescanDurationStatus_SUCCESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype EcrRescanDurationStatus = EcrRescanDurationStatus'
  { fromEcrRescanDurationStatus ::
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

pattern EcrRescanDurationStatus_FAILED :: EcrRescanDurationStatus
pattern EcrRescanDurationStatus_FAILED = EcrRescanDurationStatus' "FAILED"

pattern EcrRescanDurationStatus_PENDING :: EcrRescanDurationStatus
pattern EcrRescanDurationStatus_PENDING = EcrRescanDurationStatus' "PENDING"

pattern EcrRescanDurationStatus_SUCCESS :: EcrRescanDurationStatus
pattern EcrRescanDurationStatus_SUCCESS = EcrRescanDurationStatus' "SUCCESS"

{-# COMPLETE
  EcrRescanDurationStatus_FAILED,
  EcrRescanDurationStatus_PENDING,
  EcrRescanDurationStatus_SUCCESS,
  EcrRescanDurationStatus'
  #-}
