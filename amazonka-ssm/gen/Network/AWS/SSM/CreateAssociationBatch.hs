{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SSM.CreateAssociationBatch
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Associates the specified configuration documents with the specified instances.
--
-- When you associate a configuration document with an instance, the
-- configuration agent on the instance processes the configuration document and
-- configures the instance as specified.
--
-- If you associate a configuration document with an instance that already has
-- an associated configuration document, we replace the current configuration
-- document with the new configuration document.
--
-- <http://docs.aws.amazon.com/ssm/latest/APIReference/API_CreateAssociationBatch.html>
module Network.AWS.SSM.CreateAssociationBatch
    (
    -- * Request
      CreateAssociationBatch
    -- ** Request constructor
    , createAssociationBatch
    -- ** Request lenses
    , cabEntries

    -- * Response
    , CreateAssociationBatchResponse
    -- ** Response constructor
    , createAssociationBatchResponse
    -- ** Response lenses
    , cabrFailed
    , cabrSuccessful
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.SSM.Types
import qualified GHC.Exts

newtype CreateAssociationBatch = CreateAssociationBatch
    { _cabEntries :: List "entries" CreateAssociationBatchRequestEntry
    } deriving (Eq, Read, Show, Monoid, Semigroup)

instance GHC.Exts.IsList CreateAssociationBatch where
    type Item CreateAssociationBatch = CreateAssociationBatchRequestEntry

    fromList = CreateAssociationBatch . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _cabEntries

-- | 'CreateAssociationBatch' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cabEntries' @::@ ['CreateAssociationBatchRequestEntry']
--
createAssociationBatch :: CreateAssociationBatch
createAssociationBatch = CreateAssociationBatch
    { _cabEntries = mempty
    }

-- | One or more associations.
cabEntries :: Lens' CreateAssociationBatch [CreateAssociationBatchRequestEntry]
cabEntries = lens _cabEntries (\s a -> s { _cabEntries = a }) . _List

data CreateAssociationBatchResponse = CreateAssociationBatchResponse
    { _cabrFailed     :: List "FailedCreateAssociationEntry" FailedCreateAssociation
    , _cabrSuccessful :: List "AssociationDescription" AssociationDescription
    } deriving (Eq, Read, Show)

-- | 'CreateAssociationBatchResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cabrFailed' @::@ ['FailedCreateAssociation']
--
-- * 'cabrSuccessful' @::@ ['AssociationDescription']
--
createAssociationBatchResponse :: CreateAssociationBatchResponse
createAssociationBatchResponse = CreateAssociationBatchResponse
    { _cabrSuccessful = mempty
    , _cabrFailed     = mempty
    }

-- | Information about the associations that failed.
cabrFailed :: Lens' CreateAssociationBatchResponse [FailedCreateAssociation]
cabrFailed = lens _cabrFailed (\s a -> s { _cabrFailed = a }) . _List

-- | Information about the associations that succeeded.
cabrSuccessful :: Lens' CreateAssociationBatchResponse [AssociationDescription]
cabrSuccessful = lens _cabrSuccessful (\s a -> s { _cabrSuccessful = a }) . _List

instance ToPath CreateAssociationBatch where
    toPath = const "/"

instance ToQuery CreateAssociationBatch where
    toQuery = const mempty

instance ToHeaders CreateAssociationBatch

instance ToJSON CreateAssociationBatch where
    toJSON CreateAssociationBatch{..} = object
        [ "Entries" .= _cabEntries
        ]

instance AWSRequest CreateAssociationBatch where
    type Sv CreateAssociationBatch = SSM
    type Rs CreateAssociationBatch = CreateAssociationBatchResponse

    request  = post "CreateAssociationBatch"
    response = jsonResponse

instance FromJSON CreateAssociationBatchResponse where
    parseJSON = withObject "CreateAssociationBatchResponse" $ \o -> CreateAssociationBatchResponse
        <$> o .:? "Failed" .!= mempty
        <*> o .:? "Successful" .!= mempty
