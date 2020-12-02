{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Types.NodeAssociationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorksCM.Types.NodeAssociationStatus where

import Network.AWS.Prelude

-- | The status of the association or disassociation request.
--
--
-- __Possible values:__
--
--     * @SUCCESS@ : The association or disassociation succeeded.
--
--     * @FAILED@ : The association or disassociation failed.
--
--     * @IN_PROGRESS@ : The association or disassociation is still in progress.
data NodeAssociationStatus
  = NASFailed
  | NASInProgress
  | NASSuccess
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText NodeAssociationStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure NASFailed
      "in_progress" -> pure NASInProgress
      "success" -> pure NASSuccess
      e ->
        fromTextError $
          "Failure parsing NodeAssociationStatus from value: '" <> e
            <> "'. Accepted values: failed, in_progress, success"

instance ToText NodeAssociationStatus where
  toText = \case
    NASFailed -> "FAILED"
    NASInProgress -> "IN_PROGRESS"
    NASSuccess -> "SUCCESS"

instance Hashable NodeAssociationStatus

instance NFData NodeAssociationStatus

instance ToByteString NodeAssociationStatus

instance ToQuery NodeAssociationStatus

instance ToHeader NodeAssociationStatus

instance FromJSON NodeAssociationStatus where
  parseJSON = parseJSONText "NodeAssociationStatus"
