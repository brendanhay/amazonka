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
-- Module      : Network.AWS.ServiceCatalog.Types.ServiceActionAssociationErrorCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ServiceActionAssociationErrorCode
  ( ServiceActionAssociationErrorCode
      ( ..,
        ServiceActionAssociationErrorCode_DUPLICATE_RESOURCE,
        ServiceActionAssociationErrorCode_INTERNAL_FAILURE,
        ServiceActionAssociationErrorCode_LIMIT_EXCEEDED,
        ServiceActionAssociationErrorCode_RESOURCE_NOT_FOUND,
        ServiceActionAssociationErrorCode_THROTTLING
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ServiceActionAssociationErrorCode = ServiceActionAssociationErrorCode'
  { fromServiceActionAssociationErrorCode ::
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

pattern ServiceActionAssociationErrorCode_DUPLICATE_RESOURCE :: ServiceActionAssociationErrorCode
pattern ServiceActionAssociationErrorCode_DUPLICATE_RESOURCE = ServiceActionAssociationErrorCode' "DUPLICATE_RESOURCE"

pattern ServiceActionAssociationErrorCode_INTERNAL_FAILURE :: ServiceActionAssociationErrorCode
pattern ServiceActionAssociationErrorCode_INTERNAL_FAILURE = ServiceActionAssociationErrorCode' "INTERNAL_FAILURE"

pattern ServiceActionAssociationErrorCode_LIMIT_EXCEEDED :: ServiceActionAssociationErrorCode
pattern ServiceActionAssociationErrorCode_LIMIT_EXCEEDED = ServiceActionAssociationErrorCode' "LIMIT_EXCEEDED"

pattern ServiceActionAssociationErrorCode_RESOURCE_NOT_FOUND :: ServiceActionAssociationErrorCode
pattern ServiceActionAssociationErrorCode_RESOURCE_NOT_FOUND = ServiceActionAssociationErrorCode' "RESOURCE_NOT_FOUND"

pattern ServiceActionAssociationErrorCode_THROTTLING :: ServiceActionAssociationErrorCode
pattern ServiceActionAssociationErrorCode_THROTTLING = ServiceActionAssociationErrorCode' "THROTTLING"

{-# COMPLETE
  ServiceActionAssociationErrorCode_DUPLICATE_RESOURCE,
  ServiceActionAssociationErrorCode_INTERNAL_FAILURE,
  ServiceActionAssociationErrorCode_LIMIT_EXCEEDED,
  ServiceActionAssociationErrorCode_RESOURCE_NOT_FOUND,
  ServiceActionAssociationErrorCode_THROTTLING,
  ServiceActionAssociationErrorCode'
  #-}
