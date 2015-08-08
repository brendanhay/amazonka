{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.CreateAssociationBatch
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified configuration documents with the specified
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
-- /See:/ <http://docs.aws.amazon.com/ssm/latest/APIReference/API_CreateAssociationBatch.html AWS API Reference> for CreateAssociationBatch.
module Network.AWS.SSM.CreateAssociationBatch
    (
    -- * Creating a Request
      CreateAssociationBatch
    , createAssociationBatch
    -- * Request Lenses
    , cabEntries

    -- * Destructuring the Response
    , CreateAssociationBatchResponse
    , createAssociationBatchResponse
    -- * Response Lenses
    , cabrsSuccessful
    , cabrsFailed
    , cabrsStatus
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateAssociationBatch' smart constructor.
createAssociationBatch :: CreateAssociationBatch
createAssociationBatch =
    CreateAssociationBatch'
    { _cabEntries = mempty
    }

-- | One or more associations.
cabEntries :: Lens' CreateAssociationBatch [CreateAssociationBatchRequestEntry]
cabEntries = lens _cabEntries (\ s a -> s{_cabEntries = a}) . _Coerce;

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
-- * 'cabrsSuccessful'
--
-- * 'cabrsFailed'
--
-- * 'cabrsStatus'
data CreateAssociationBatchResponse = CreateAssociationBatchResponse'
    { _cabrsSuccessful :: !(Maybe [AssociationDescription])
    , _cabrsFailed     :: !(Maybe [FailedCreateAssociation])
    , _cabrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateAssociationBatchResponse' smart constructor.
createAssociationBatchResponse :: Int -> CreateAssociationBatchResponse
createAssociationBatchResponse pStatus_ =
    CreateAssociationBatchResponse'
    { _cabrsSuccessful = Nothing
    , _cabrsFailed = Nothing
    , _cabrsStatus = pStatus_
    }

-- | Information about the associations that succeeded.
cabrsSuccessful :: Lens' CreateAssociationBatchResponse [AssociationDescription]
cabrsSuccessful = lens _cabrsSuccessful (\ s a -> s{_cabrsSuccessful = a}) . _Default . _Coerce;

-- | Information about the associations that failed.
cabrsFailed :: Lens' CreateAssociationBatchResponse [FailedCreateAssociation]
cabrsFailed = lens _cabrsFailed (\ s a -> s{_cabrsFailed = a}) . _Default . _Coerce;

-- | Undocumented member.
cabrsStatus :: Lens' CreateAssociationBatchResponse Int
cabrsStatus = lens _cabrsStatus (\ s a -> s{_cabrsStatus = a});
