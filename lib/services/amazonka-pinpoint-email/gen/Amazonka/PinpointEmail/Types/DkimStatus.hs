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
-- Module      : Amazonka.PinpointEmail.Types.DkimStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointEmail.Types.DkimStatus
  ( DkimStatus
      ( ..,
        DkimStatus_FAILED,
        DkimStatus_NOT_STARTED,
        DkimStatus_PENDING,
        DkimStatus_SUCCESS,
        DkimStatus_TEMPORARY_FAILURE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The DKIM authentication status of the identity. The status can be one of
-- the following:
--
-- -   @PENDING@ – The DKIM verification process was initiated, and Amazon
--     Pinpoint is still waiting for the required CNAME records to appear
--     in the DNS configuration for the domain.
--
-- -   @SUCCESS@ – The DKIM authentication process completed successfully.
--
-- -   @FAILED@ – The DKIM authentication process failed. This can happen
--     when Amazon Pinpoint fails to find the required CNAME records in the
--     DNS configuration of the domain.
--
-- -   @TEMPORARY_FAILURE@ – A temporary issue is preventing Amazon
--     Pinpoint from determining the DKIM authentication status of the
--     domain.
--
-- -   @NOT_STARTED@ – The DKIM verification process hasn\'t been initiated
--     for the domain.
newtype DkimStatus = DkimStatus'
  { fromDkimStatus ::
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

pattern DkimStatus_FAILED :: DkimStatus
pattern DkimStatus_FAILED = DkimStatus' "FAILED"

pattern DkimStatus_NOT_STARTED :: DkimStatus
pattern DkimStatus_NOT_STARTED = DkimStatus' "NOT_STARTED"

pattern DkimStatus_PENDING :: DkimStatus
pattern DkimStatus_PENDING = DkimStatus' "PENDING"

pattern DkimStatus_SUCCESS :: DkimStatus
pattern DkimStatus_SUCCESS = DkimStatus' "SUCCESS"

pattern DkimStatus_TEMPORARY_FAILURE :: DkimStatus
pattern DkimStatus_TEMPORARY_FAILURE = DkimStatus' "TEMPORARY_FAILURE"

{-# COMPLETE
  DkimStatus_FAILED,
  DkimStatus_NOT_STARTED,
  DkimStatus_PENDING,
  DkimStatus_SUCCESS,
  DkimStatus_TEMPORARY_FAILURE,
  DkimStatus'
  #-}
