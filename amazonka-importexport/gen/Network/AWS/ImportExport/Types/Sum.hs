{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImportExport.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ImportExport.Types.Sum where

import Network.AWS.Prelude

-- | Specifies whether the job to initiate is an import or export job.
data JobType
  = Export
  | Import
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText JobType where
    parser = takeLowerText >>= \case
        "export" -> pure Export
        "import" -> pure Import
        e -> fromTextError $ "Failure parsing JobType from value: '" <> e
           <> "'. Accepted values: export, import"

instance ToText JobType where
    toText = \case
        Export -> "Export"
        Import -> "Import"

instance Hashable     JobType
instance NFData       JobType
instance ToByteString JobType
instance ToQuery      JobType
instance ToHeader     JobType

instance FromXML JobType where
    parseXML = parseXMLText "JobType"
