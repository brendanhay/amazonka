{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Types.NodeAssociationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorksCM.Types.NodeAssociationStatus
  ( NodeAssociationStatus
      ( ..,
        NodeAssociationStatus_FAILED,
        NodeAssociationStatus_IN_PROGRESS,
        NodeAssociationStatus_SUCCESS
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | The status of the association or disassociation request.
--
-- __Possible values:__
--
-- -   @SUCCESS@: The association or disassociation succeeded.
--
-- -   @FAILED@: The association or disassociation failed.
--
-- -   @IN_PROGRESS@: The association or disassociation is still in
--     progress.
newtype NodeAssociationStatus = NodeAssociationStatus'
  { fromNodeAssociationStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern NodeAssociationStatus_FAILED :: NodeAssociationStatus
pattern NodeAssociationStatus_FAILED = NodeAssociationStatus' "FAILED"

pattern NodeAssociationStatus_IN_PROGRESS :: NodeAssociationStatus
pattern NodeAssociationStatus_IN_PROGRESS = NodeAssociationStatus' "IN_PROGRESS"

pattern NodeAssociationStatus_SUCCESS :: NodeAssociationStatus
pattern NodeAssociationStatus_SUCCESS = NodeAssociationStatus' "SUCCESS"

{-# COMPLETE
  NodeAssociationStatus_FAILED,
  NodeAssociationStatus_IN_PROGRESS,
  NodeAssociationStatus_SUCCESS,
  NodeAssociationStatus'
  #-}
