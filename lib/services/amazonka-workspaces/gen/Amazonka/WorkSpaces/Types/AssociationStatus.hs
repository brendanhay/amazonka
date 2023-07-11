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
-- Module      : Amazonka.WorkSpaces.Types.AssociationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.AssociationStatus
  ( AssociationStatus
      ( ..,
        AssociationStatus_ASSOCIATED_WITH_OWNER_ACCOUNT,
        AssociationStatus_ASSOCIATED_WITH_SHARED_ACCOUNT,
        AssociationStatus_NOT_ASSOCIATED,
        AssociationStatus_PENDING_ASSOCIATION,
        AssociationStatus_PENDING_DISASSOCIATION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AssociationStatus = AssociationStatus'
  { fromAssociationStatus ::
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

pattern AssociationStatus_ASSOCIATED_WITH_OWNER_ACCOUNT :: AssociationStatus
pattern AssociationStatus_ASSOCIATED_WITH_OWNER_ACCOUNT = AssociationStatus' "ASSOCIATED_WITH_OWNER_ACCOUNT"

pattern AssociationStatus_ASSOCIATED_WITH_SHARED_ACCOUNT :: AssociationStatus
pattern AssociationStatus_ASSOCIATED_WITH_SHARED_ACCOUNT = AssociationStatus' "ASSOCIATED_WITH_SHARED_ACCOUNT"

pattern AssociationStatus_NOT_ASSOCIATED :: AssociationStatus
pattern AssociationStatus_NOT_ASSOCIATED = AssociationStatus' "NOT_ASSOCIATED"

pattern AssociationStatus_PENDING_ASSOCIATION :: AssociationStatus
pattern AssociationStatus_PENDING_ASSOCIATION = AssociationStatus' "PENDING_ASSOCIATION"

pattern AssociationStatus_PENDING_DISASSOCIATION :: AssociationStatus
pattern AssociationStatus_PENDING_DISASSOCIATION = AssociationStatus' "PENDING_DISASSOCIATION"

{-# COMPLETE
  AssociationStatus_ASSOCIATED_WITH_OWNER_ACCOUNT,
  AssociationStatus_ASSOCIATED_WITH_SHARED_ACCOUNT,
  AssociationStatus_NOT_ASSOCIATED,
  AssociationStatus_PENDING_ASSOCIATION,
  AssociationStatus_PENDING_DISASSOCIATION,
  AssociationStatus'
  #-}
