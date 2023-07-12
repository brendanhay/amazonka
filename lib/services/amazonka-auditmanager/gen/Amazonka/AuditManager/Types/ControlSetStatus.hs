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
-- Module      : Amazonka.AuditManager.Types.ControlSetStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.ControlSetStatus
  ( ControlSetStatus
      ( ..,
        ControlSetStatus_ACTIVE,
        ControlSetStatus_REVIEWED,
        ControlSetStatus_UNDER_REVIEW
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ControlSetStatus = ControlSetStatus'
  { fromControlSetStatus ::
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

pattern ControlSetStatus_ACTIVE :: ControlSetStatus
pattern ControlSetStatus_ACTIVE = ControlSetStatus' "ACTIVE"

pattern ControlSetStatus_REVIEWED :: ControlSetStatus
pattern ControlSetStatus_REVIEWED = ControlSetStatus' "REVIEWED"

pattern ControlSetStatus_UNDER_REVIEW :: ControlSetStatus
pattern ControlSetStatus_UNDER_REVIEW = ControlSetStatus' "UNDER_REVIEW"

{-# COMPLETE
  ControlSetStatus_ACTIVE,
  ControlSetStatus_REVIEWED,
  ControlSetStatus_UNDER_REVIEW,
  ControlSetStatus'
  #-}
