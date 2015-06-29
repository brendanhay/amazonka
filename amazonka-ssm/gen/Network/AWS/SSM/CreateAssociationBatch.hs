{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SSM.CreateAssociationBatch
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Associates the specified configuration documents with the specified
-- instances.
--
-- When you associate a configuration document with an instance, the
-- configuration agent on the instance processes the configuration document
-- and configures the instance as specified.
--
-- If you associate a configuration document with an instance that already
-- has an associated configuration document, we replace the current
-- configuration document with the new configuration document.
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
    , cabrSuccessful
    , cabrFailed
    , cabrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SSM.Types

-- | /See:/ 'createAssociationBatch' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cabEntries'
newtype CreateAssociationBatch = CreateAssociationBatch'
    { _cabEntries :: [CreateAssociationBatchRequestEntry]
    } deriving (Eq,Read,Show)

-- | 'CreateAssociationBatch' smart constructor.
createAssociationBatch :: CreateAssociationBatch
createAssociationBatch =
    CreateAssociationBatch'
    { _cabEntries = mempty
    }

-- | One or more associations.
cabEntries :: Lens' CreateAssociationBatch [CreateAssociationBatchRequestEntry]
cabEntries = lens _cabEntries (\ s a -> s{_cabEntries = a});

instance AWSRequest CreateAssociationBatch where
        type Sv CreateAssociationBatch = SSM
        type Rs CreateAssociationBatch =
             CreateAssociationBatchResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateAssociationBatchResponse' <$>
                   (x .?> "Successful" .!@ mempty) <*>
                     (x .?> "Failed" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders CreateAssociationBatch where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.CreateAssociationBatch" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateAssociationBatch where
        toJSON CreateAssociationBatch'{..}
          = object ["Entries" .= _cabEntries]

instance ToPath CreateAssociationBatch where
        toPath = const "/"

instance ToQuery CreateAssociationBatch where
        toQuery = const mempty

-- | /See:/ 'createAssociationBatchResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cabrSuccessful'
--
-- * 'cabrFailed'
--
-- * 'cabrStatus'
data CreateAssociationBatchResponse = CreateAssociationBatchResponse'
    { _cabrSuccessful :: !(Maybe [AssociationDescription])
    , _cabrFailed     :: !(Maybe [FailedCreateAssociation])
    , _cabrStatus     :: !Int
    } deriving (Eq,Read,Show)

-- | 'CreateAssociationBatchResponse' smart constructor.
createAssociationBatchResponse :: Int -> CreateAssociationBatchResponse
createAssociationBatchResponse pStatus =
    CreateAssociationBatchResponse'
    { _cabrSuccessful = Nothing
    , _cabrFailed = Nothing
    , _cabrStatus = pStatus
    }

-- | Information about the associations that succeeded.
cabrSuccessful :: Lens' CreateAssociationBatchResponse [AssociationDescription]
cabrSuccessful = lens _cabrSuccessful (\ s a -> s{_cabrSuccessful = a}) . _Default;

-- | Information about the associations that failed.
cabrFailed :: Lens' CreateAssociationBatchResponse [FailedCreateAssociation]
cabrFailed = lens _cabrFailed (\ s a -> s{_cabrFailed = a}) . _Default;

-- | FIXME: Undocumented member.
cabrStatus :: Lens' CreateAssociationBatchResponse Int
cabrStatus = lens _cabrStatus (\ s a -> s{_cabrStatus = a});
