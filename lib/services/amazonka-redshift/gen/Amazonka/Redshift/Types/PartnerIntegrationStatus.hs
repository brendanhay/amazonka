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
-- Module      : Amazonka.Redshift.Types.PartnerIntegrationStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.PartnerIntegrationStatus
  ( PartnerIntegrationStatus
      ( ..,
        PartnerIntegrationStatus_Active,
        PartnerIntegrationStatus_ConnectionFailure,
        PartnerIntegrationStatus_Inactive,
        PartnerIntegrationStatus_RuntimeFailure
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

newtype PartnerIntegrationStatus = PartnerIntegrationStatus'
  { fromPartnerIntegrationStatus ::
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

pattern PartnerIntegrationStatus_Active :: PartnerIntegrationStatus
pattern PartnerIntegrationStatus_Active = PartnerIntegrationStatus' "Active"

pattern PartnerIntegrationStatus_ConnectionFailure :: PartnerIntegrationStatus
pattern PartnerIntegrationStatus_ConnectionFailure = PartnerIntegrationStatus' "ConnectionFailure"

pattern PartnerIntegrationStatus_Inactive :: PartnerIntegrationStatus
pattern PartnerIntegrationStatus_Inactive = PartnerIntegrationStatus' "Inactive"

pattern PartnerIntegrationStatus_RuntimeFailure :: PartnerIntegrationStatus
pattern PartnerIntegrationStatus_RuntimeFailure = PartnerIntegrationStatus' "RuntimeFailure"

{-# COMPLETE
  PartnerIntegrationStatus_Active,
  PartnerIntegrationStatus_ConnectionFailure,
  PartnerIntegrationStatus_Inactive,
  PartnerIntegrationStatus_RuntimeFailure,
  PartnerIntegrationStatus'
  #-}
