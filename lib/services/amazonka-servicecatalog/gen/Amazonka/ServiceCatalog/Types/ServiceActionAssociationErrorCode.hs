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
-- Module      : Amazonka.ServiceCatalog.Types.ServiceActionAssociationErrorCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ServiceActionAssociationErrorCode
  ( ServiceActionAssociationErrorCode
      ( ..,
        ServiceActionAssociationErrorCode_DUPLICATE_RESOURCE,
        ServiceActionAssociationErrorCode_INTERNAL_FAILURE,
        ServiceActionAssociationErrorCode_INVALID_PARAMETER,
        ServiceActionAssociationErrorCode_LIMIT_EXCEEDED,
        ServiceActionAssociationErrorCode_RESOURCE_NOT_FOUND,
        ServiceActionAssociationErrorCode_THROTTLING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ServiceActionAssociationErrorCode = ServiceActionAssociationErrorCode'
  { fromServiceActionAssociationErrorCode ::
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

pattern ServiceActionAssociationErrorCode_DUPLICATE_RESOURCE :: ServiceActionAssociationErrorCode
pattern ServiceActionAssociationErrorCode_DUPLICATE_RESOURCE = ServiceActionAssociationErrorCode' "DUPLICATE_RESOURCE"

pattern ServiceActionAssociationErrorCode_INTERNAL_FAILURE :: ServiceActionAssociationErrorCode
pattern ServiceActionAssociationErrorCode_INTERNAL_FAILURE = ServiceActionAssociationErrorCode' "INTERNAL_FAILURE"

pattern ServiceActionAssociationErrorCode_INVALID_PARAMETER :: ServiceActionAssociationErrorCode
pattern ServiceActionAssociationErrorCode_INVALID_PARAMETER = ServiceActionAssociationErrorCode' "INVALID_PARAMETER"

pattern ServiceActionAssociationErrorCode_LIMIT_EXCEEDED :: ServiceActionAssociationErrorCode
pattern ServiceActionAssociationErrorCode_LIMIT_EXCEEDED = ServiceActionAssociationErrorCode' "LIMIT_EXCEEDED"

pattern ServiceActionAssociationErrorCode_RESOURCE_NOT_FOUND :: ServiceActionAssociationErrorCode
pattern ServiceActionAssociationErrorCode_RESOURCE_NOT_FOUND = ServiceActionAssociationErrorCode' "RESOURCE_NOT_FOUND"

pattern ServiceActionAssociationErrorCode_THROTTLING :: ServiceActionAssociationErrorCode
pattern ServiceActionAssociationErrorCode_THROTTLING = ServiceActionAssociationErrorCode' "THROTTLING"

{-# COMPLETE
  ServiceActionAssociationErrorCode_DUPLICATE_RESOURCE,
  ServiceActionAssociationErrorCode_INTERNAL_FAILURE,
  ServiceActionAssociationErrorCode_INVALID_PARAMETER,
  ServiceActionAssociationErrorCode_LIMIT_EXCEEDED,
  ServiceActionAssociationErrorCode_RESOURCE_NOT_FOUND,
  ServiceActionAssociationErrorCode_THROTTLING,
  ServiceActionAssociationErrorCode'
  #-}
