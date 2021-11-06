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
-- Module      : Amazonka.ManagedBlockChain.Types.ProposalStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.ProposalStatus
  ( ProposalStatus
      ( ..,
        ProposalStatus_ACTION_FAILED,
        ProposalStatus_APPROVED,
        ProposalStatus_EXPIRED,
        ProposalStatus_IN_PROGRESS,
        ProposalStatus_REJECTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ProposalStatus = ProposalStatus'
  { fromProposalStatus ::
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

pattern ProposalStatus_ACTION_FAILED :: ProposalStatus
pattern ProposalStatus_ACTION_FAILED = ProposalStatus' "ACTION_FAILED"

pattern ProposalStatus_APPROVED :: ProposalStatus
pattern ProposalStatus_APPROVED = ProposalStatus' "APPROVED"

pattern ProposalStatus_EXPIRED :: ProposalStatus
pattern ProposalStatus_EXPIRED = ProposalStatus' "EXPIRED"

pattern ProposalStatus_IN_PROGRESS :: ProposalStatus
pattern ProposalStatus_IN_PROGRESS = ProposalStatus' "IN_PROGRESS"

pattern ProposalStatus_REJECTED :: ProposalStatus
pattern ProposalStatus_REJECTED = ProposalStatus' "REJECTED"

{-# COMPLETE
  ProposalStatus_ACTION_FAILED,
  ProposalStatus_APPROVED,
  ProposalStatus_EXPIRED,
  ProposalStatus_IN_PROGRESS,
  ProposalStatus_REJECTED,
  ProposalStatus'
  #-}
