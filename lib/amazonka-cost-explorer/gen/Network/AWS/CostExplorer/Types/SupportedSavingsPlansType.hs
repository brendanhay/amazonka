{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SupportedSavingsPlansType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SupportedSavingsPlansType where

import Network.AWS.Prelude

data SupportedSavingsPlansType
  = ComputeSp
  | EC2InstanceSp
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

instance FromText SupportedSavingsPlansType where
  parser =
    takeLowerText >>= \case
      "compute_sp" -> pure ComputeSp
      "ec2_instance_sp" -> pure EC2InstanceSp
      e ->
        fromTextError $
          "Failure parsing SupportedSavingsPlansType from value: '" <> e
            <> "'. Accepted values: compute_sp, ec2_instance_sp"

instance ToText SupportedSavingsPlansType where
  toText = \case
    ComputeSp -> "COMPUTE_SP"
    EC2InstanceSp -> "EC2_INSTANCE_SP"

instance Hashable SupportedSavingsPlansType

instance NFData SupportedSavingsPlansType

instance ToByteString SupportedSavingsPlansType

instance ToQuery SupportedSavingsPlansType

instance ToHeader SupportedSavingsPlansType

instance ToJSON SupportedSavingsPlansType where
  toJSON = toJSONText

instance FromJSON SupportedSavingsPlansType where
  parseJSON = parseJSONText "SupportedSavingsPlansType"
