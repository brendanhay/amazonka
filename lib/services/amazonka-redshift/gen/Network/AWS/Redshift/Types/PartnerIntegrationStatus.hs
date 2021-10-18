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
-- Module      : Network.AWS.Redshift.Types.PartnerIntegrationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.PartnerIntegrationStatus
  ( PartnerIntegrationStatus
      ( ..,
        PartnerIntegrationStatus_Active,
        PartnerIntegrationStatus_ConnectionFailure,
        PartnerIntegrationStatus_Inactive,
        PartnerIntegrationStatus_RuntimeFailure
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal

newtype PartnerIntegrationStatus = PartnerIntegrationStatus'
  { fromPartnerIntegrationStatus ::
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
