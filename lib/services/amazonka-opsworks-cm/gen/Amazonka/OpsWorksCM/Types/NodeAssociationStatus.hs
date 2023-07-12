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
-- Module      : Amazonka.OpsWorksCM.Types.NodeAssociationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorksCM.Types.NodeAssociationStatus
  ( NodeAssociationStatus
      ( ..,
        NodeAssociationStatus_FAILED,
        NodeAssociationStatus_IN_PROGRESS,
        NodeAssociationStatus_SUCCESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
