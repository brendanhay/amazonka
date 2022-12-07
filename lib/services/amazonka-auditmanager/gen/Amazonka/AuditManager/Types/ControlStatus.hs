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
-- Module      : Amazonka.AuditManager.Types.ControlStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.ControlStatus
  ( ControlStatus
      ( ..,
        ControlStatus_INACTIVE,
        ControlStatus_REVIEWED,
        ControlStatus_UNDER_REVIEW
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ControlStatus = ControlStatus'
  { fromControlStatus ::
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

pattern ControlStatus_INACTIVE :: ControlStatus
pattern ControlStatus_INACTIVE = ControlStatus' "INACTIVE"

pattern ControlStatus_REVIEWED :: ControlStatus
pattern ControlStatus_REVIEWED = ControlStatus' "REVIEWED"

pattern ControlStatus_UNDER_REVIEW :: ControlStatus
pattern ControlStatus_UNDER_REVIEW = ControlStatus' "UNDER_REVIEW"

{-# COMPLETE
  ControlStatus_INACTIVE,
  ControlStatus_REVIEWED,
  ControlStatus_UNDER_REVIEW,
  ControlStatus'
  #-}
