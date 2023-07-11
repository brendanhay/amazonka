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
-- Module      : Amazonka.ServiceQuotas.Types.ServiceQuotaTemplateAssociationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceQuotas.Types.ServiceQuotaTemplateAssociationStatus
  ( ServiceQuotaTemplateAssociationStatus
      ( ..,
        ServiceQuotaTemplateAssociationStatus_ASSOCIATED,
        ServiceQuotaTemplateAssociationStatus_DISASSOCIATED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ServiceQuotaTemplateAssociationStatus = ServiceQuotaTemplateAssociationStatus'
  { fromServiceQuotaTemplateAssociationStatus ::
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

pattern ServiceQuotaTemplateAssociationStatus_ASSOCIATED :: ServiceQuotaTemplateAssociationStatus
pattern ServiceQuotaTemplateAssociationStatus_ASSOCIATED = ServiceQuotaTemplateAssociationStatus' "ASSOCIATED"

pattern ServiceQuotaTemplateAssociationStatus_DISASSOCIATED :: ServiceQuotaTemplateAssociationStatus
pattern ServiceQuotaTemplateAssociationStatus_DISASSOCIATED = ServiceQuotaTemplateAssociationStatus' "DISASSOCIATED"

{-# COMPLETE
  ServiceQuotaTemplateAssociationStatus_ASSOCIATED,
  ServiceQuotaTemplateAssociationStatus_DISASSOCIATED,
  ServiceQuotaTemplateAssociationStatus'
  #-}
