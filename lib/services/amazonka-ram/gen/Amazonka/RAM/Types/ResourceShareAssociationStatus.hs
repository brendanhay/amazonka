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
-- Module      : Amazonka.RAM.Types.ResourceShareAssociationStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RAM.Types.ResourceShareAssociationStatus
  ( ResourceShareAssociationStatus
      ( ..,
        ResourceShareAssociationStatus_ASSOCIATED,
        ResourceShareAssociationStatus_ASSOCIATING,
        ResourceShareAssociationStatus_DISASSOCIATED,
        ResourceShareAssociationStatus_DISASSOCIATING,
        ResourceShareAssociationStatus_FAILED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResourceShareAssociationStatus = ResourceShareAssociationStatus'
  { fromResourceShareAssociationStatus ::
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

pattern ResourceShareAssociationStatus_ASSOCIATED :: ResourceShareAssociationStatus
pattern ResourceShareAssociationStatus_ASSOCIATED = ResourceShareAssociationStatus' "ASSOCIATED"

pattern ResourceShareAssociationStatus_ASSOCIATING :: ResourceShareAssociationStatus
pattern ResourceShareAssociationStatus_ASSOCIATING = ResourceShareAssociationStatus' "ASSOCIATING"

pattern ResourceShareAssociationStatus_DISASSOCIATED :: ResourceShareAssociationStatus
pattern ResourceShareAssociationStatus_DISASSOCIATED = ResourceShareAssociationStatus' "DISASSOCIATED"

pattern ResourceShareAssociationStatus_DISASSOCIATING :: ResourceShareAssociationStatus
pattern ResourceShareAssociationStatus_DISASSOCIATING = ResourceShareAssociationStatus' "DISASSOCIATING"

pattern ResourceShareAssociationStatus_FAILED :: ResourceShareAssociationStatus
pattern ResourceShareAssociationStatus_FAILED = ResourceShareAssociationStatus' "FAILED"

{-# COMPLETE
  ResourceShareAssociationStatus_ASSOCIATED,
  ResourceShareAssociationStatus_ASSOCIATING,
  ResourceShareAssociationStatus_DISASSOCIATED,
  ResourceShareAssociationStatus_DISASSOCIATING,
  ResourceShareAssociationStatus_FAILED,
  ResourceShareAssociationStatus'
  #-}
