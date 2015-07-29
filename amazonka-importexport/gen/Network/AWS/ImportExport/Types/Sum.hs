{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImportExport.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ImportExport.Types.Sum where

import           Network.AWS.Prelude

-- | Specifies whether the job to initiate is an import or export job.
data JobType
    = Export
    | Import
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText JobType where
    parser = takeLowerText >>= \case
        "export" -> pure Export
        "import" -> pure Import
        e -> fromTextError $ "Failure parsing JobType from value: '" <> e
           <> "'. Accepted values: export, import"

instance ToText JobType where
    toText = \case
        Export -> "export"
        Import -> "import"

instance Hashable     JobType
instance ToByteString JobType
instance ToQuery      JobType
instance ToHeader     JobType

instance FromXML JobType where
    parseXML = parseXMLText "JobType"
