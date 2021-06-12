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

newtype AssociationStatus = AssociationStatus'
  { fromAssociationStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
