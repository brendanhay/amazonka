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
-- Module      : Amazonka.MechanicalTurk.Types.AssignmentStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MechanicalTurk.Types.AssignmentStatus
  ( AssignmentStatus
      ( ..,
        AssignmentStatus_Approved,
        AssignmentStatus_Rejected,
        AssignmentStatus_Submitted
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AssignmentStatus = AssignmentStatus'
  { fromAssignmentStatus ::
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

pattern AssignmentStatus_Approved :: AssignmentStatus
pattern AssignmentStatus_Approved = AssignmentStatus' "Approved"

pattern AssignmentStatus_Rejected :: AssignmentStatus
pattern AssignmentStatus_Rejected = AssignmentStatus' "Rejected"

pattern AssignmentStatus_Submitted :: AssignmentStatus
pattern AssignmentStatus_Submitted = AssignmentStatus' "Submitted"

{-# COMPLETE
  AssignmentStatus_Approved,
  AssignmentStatus_Rejected,
  AssignmentStatus_Submitted,
  AssignmentStatus'
  #-}
