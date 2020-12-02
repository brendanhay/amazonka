{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.OnFailure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.OnFailure where

import Network.AWS.Prelude

data OnFailure
  = OFDelete
  | OFDoNothing
  | OFRollback
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

instance FromText OnFailure where
  parser =
    takeLowerText >>= \case
      "delete" -> pure OFDelete
      "do_nothing" -> pure OFDoNothing
      "rollback" -> pure OFRollback
      e ->
        fromTextError $
          "Failure parsing OnFailure from value: '" <> e
            <> "'. Accepted values: delete, do_nothing, rollback"

instance ToText OnFailure where
  toText = \case
    OFDelete -> "DELETE"
    OFDoNothing -> "DO_NOTHING"
    OFRollback -> "ROLLBACK"

instance Hashable OnFailure

instance NFData OnFailure

instance ToByteString OnFailure

instance ToQuery OnFailure

instance ToHeader OnFailure
