{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.CreateAssociationBatch
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified Systems Manager document with the specified instances or targets.
--
--
-- When you associate a document with one or more instances using instance IDs or tags, the SSM Agent running on the instance processes the document and configures the instance as specified.
--
-- If you associate a document with an instance that already has an associated document, the system throws the AssociationAlreadyExists exception.
--
module Network.AWS.SSM.CreateAssociationBatch
    (
    -- * Creating a Request
      createAssociationBatch
    , CreateAssociationBatch
    -- * Request Lenses
    , cabEntries

    -- * Destructuring the Response
    , createAssociationBatchResponse
    , CreateAssociationBatchResponse
    -- * Response Lenses
    , cabrsSuccessful
    , cabrsFailed
    , cabrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'createAssociationBatch' smart constructor.
newtype CreateAssociationBatch = CreateAssociationBatch'
  { _cabEntries :: List1 CreateAssociationBatchRequestEntry
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAssociationBatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cabEntries' - One or more associations.
createAssociationBatch
    :: NonEmpty CreateAssociationBatchRequestEntry -- ^ 'cabEntries'
    -> CreateAssociationBatch
createAssociationBatch pEntries_ =
  CreateAssociationBatch' {_cabEntries = _List1 # pEntries_}


-- | One or more associations.
cabEntries :: Lens' CreateAssociationBatch (NonEmpty CreateAssociationBatchRequestEntry)
cabEntries = lens _cabEntries (\ s a -> s{_cabEntries = a}) . _List1

instance AWSRequest CreateAssociationBatch where
        type Rs CreateAssociationBatch =
             CreateAssociationBatchResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 CreateAssociationBatchResponse' <$>
                   (x .?> "Successful" .!@ mempty) <*>
                     (x .?> "Failed" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable CreateAssociationBatch where

instance NFData CreateAssociationBatch where

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
          = object
              (catMaybes [Just ("Entries" .= _cabEntries)])

instance ToPath CreateAssociationBatch where
        toPath = const "/"

instance ToQuery CreateAssociationBatch where
        toQuery = const mempty

-- | /See:/ 'createAssociationBatchResponse' smart constructor.
data CreateAssociationBatchResponse = CreateAssociationBatchResponse'
  { _cabrsSuccessful     :: !(Maybe [AssociationDescription])
  , _cabrsFailed         :: !(Maybe [FailedCreateAssociation])
  , _cabrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAssociationBatchResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cabrsSuccessful' - Information about the associations that succeeded.
--
-- * 'cabrsFailed' - Information about the associations that failed.
--
-- * 'cabrsResponseStatus' - -- | The response status code.
createAssociationBatchResponse
    :: Int -- ^ 'cabrsResponseStatus'
    -> CreateAssociationBatchResponse
createAssociationBatchResponse pResponseStatus_ =
  CreateAssociationBatchResponse'
    { _cabrsSuccessful = Nothing
    , _cabrsFailed = Nothing
    , _cabrsResponseStatus = pResponseStatus_
    }


-- | Information about the associations that succeeded.
cabrsSuccessful :: Lens' CreateAssociationBatchResponse [AssociationDescription]
cabrsSuccessful = lens _cabrsSuccessful (\ s a -> s{_cabrsSuccessful = a}) . _Default . _Coerce

-- | Information about the associations that failed.
cabrsFailed :: Lens' CreateAssociationBatchResponse [FailedCreateAssociation]
cabrsFailed = lens _cabrsFailed (\ s a -> s{_cabrsFailed = a}) . _Default . _Coerce

-- | -- | The response status code.
cabrsResponseStatus :: Lens' CreateAssociationBatchResponse Int
cabrsResponseStatus = lens _cabrsResponseStatus (\ s a -> s{_cabrsResponseStatus = a})

instance NFData CreateAssociationBatchResponse where
