{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ModelApprovalStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelApprovalStatus
  ( ModelApprovalStatus
      ( ..,
        ModelApprovalStatus_Approved,
        ModelApprovalStatus_PendingManualApproval,
        ModelApprovalStatus_Rejected
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ModelApprovalStatus = ModelApprovalStatus'
  { fromModelApprovalStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
