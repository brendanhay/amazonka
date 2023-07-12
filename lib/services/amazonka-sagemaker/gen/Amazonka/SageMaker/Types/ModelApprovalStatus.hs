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
-- Module      : Amazonka.SageMaker.Types.ModelApprovalStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelApprovalStatus
  ( ModelApprovalStatus
      ( ..,
        ModelApprovalStatus_Approved,
        ModelApprovalStatus_PendingManualApproval,
        ModelApprovalStatus_Rejected
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ModelApprovalStatus = ModelApprovalStatus'
  { fromModelApprovalStatus ::
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

pattern ModelApprovalStatus_Approved :: ModelApprovalStatus
pattern ModelApprovalStatus_Approved = ModelApprovalStatus' "Approved"

pattern ModelApprovalStatus_PendingManualApproval :: ModelApprovalStatus
pattern ModelApprovalStatus_PendingManualApproval = ModelApprovalStatus' "PendingManualApproval"

pattern ModelApprovalStatus_Rejected :: ModelApprovalStatus
pattern ModelApprovalStatus_Rejected = ModelApprovalStatus' "Rejected"

{-# COMPLETE
  ModelApprovalStatus_Approved,
  ModelApprovalStatus_PendingManualApproval,
  ModelApprovalStatus_Rejected,
  ModelApprovalStatus'
  #-}
