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
-- Module      : Network.AWS.WorkSpaces.Types.AssociationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.AssociationStatus
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype AssociationStatus = AssociationStatus'
  { fromAssociationStatus ::
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
