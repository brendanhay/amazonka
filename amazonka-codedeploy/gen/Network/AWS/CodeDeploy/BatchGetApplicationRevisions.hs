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
-- Module      : Network.AWS.CodeDeploy.BatchGetApplicationRevisions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more application revisions.
--
--
module Network.AWS.CodeDeploy.BatchGetApplicationRevisions
    (
    -- * Creating a Request
      batchGetApplicationRevisions
    , BatchGetApplicationRevisions
    -- * Request Lenses
    , bgarApplicationName
    , bgarRevisions

    -- * Destructuring the Response
    , batchGetApplicationRevisionsResponse
    , BatchGetApplicationRevisionsResponse
    -- * Response Lenses
    , bgarrsApplicationName
    , bgarrsRevisions
    , bgarrsErrorMessage
    , bgarrsResponseStatus
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a BatchGetApplicationRevisions operation.
--
--
--
-- /See:/ 'batchGetApplicationRevisions' smart constructor.
data BatchGetApplicationRevisions = BatchGetApplicationRevisions'
  { _bgarApplicationName :: !Text
  , _bgarRevisions       :: ![RevisionLocation]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchGetApplicationRevisions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgarApplicationName' - The name of an AWS CodeDeploy application about which to get revision information.
--
-- * 'bgarRevisions' - Information to get about the application revisions, including type and location.
batchGetApplicationRevisions
    :: Text -- ^ 'bgarApplicationName'
    -> BatchGetApplicationRevisions
batchGetApplicationRevisions pApplicationName_ =
  BatchGetApplicationRevisions'
    {_bgarApplicationName = pApplicationName_, _bgarRevisions = mempty}


-- | The name of an AWS CodeDeploy application about which to get revision information.
bgarApplicationName :: Lens' BatchGetApplicationRevisions Text
bgarApplicationName = lens _bgarApplicationName (\ s a -> s{_bgarApplicationName = a})

-- | Information to get about the application revisions, including type and location.
bgarRevisions :: Lens' BatchGetApplicationRevisions [RevisionLocation]
bgarRevisions = lens _bgarRevisions (\ s a -> s{_bgarRevisions = a}) . _Coerce

instance AWSRequest BatchGetApplicationRevisions
         where
        type Rs BatchGetApplicationRevisions =
             BatchGetApplicationRevisionsResponse
        request = postJSON codeDeploy
        response
          = receiveJSON
              (\ s h x ->
                 BatchGetApplicationRevisionsResponse' <$>
                   (x .?> "applicationName") <*>
                     (x .?> "revisions" .!@ mempty)
                     <*> (x .?> "errorMessage")
                     <*> (pure (fromEnum s)))

instance Hashable BatchGetApplicationRevisions where

instance NFData BatchGetApplicationRevisions where

instance ToHeaders BatchGetApplicationRevisions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.BatchGetApplicationRevisions"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON BatchGetApplicationRevisions where
        toJSON BatchGetApplicationRevisions'{..}
          = object
              (catMaybes
                 [Just ("applicationName" .= _bgarApplicationName),
                  Just ("revisions" .= _bgarRevisions)])

instance ToPath BatchGetApplicationRevisions where
        toPath = const "/"

instance ToQuery BatchGetApplicationRevisions where
        toQuery = const mempty

-- | Represents the output of a BatchGetApplicationRevisions operation.
--
--
--
-- /See:/ 'batchGetApplicationRevisionsResponse' smart constructor.
data BatchGetApplicationRevisionsResponse = BatchGetApplicationRevisionsResponse'
  { _bgarrsApplicationName :: !(Maybe Text)
  , _bgarrsRevisions       :: !(Maybe [RevisionInfo])
  , _bgarrsErrorMessage    :: !(Maybe Text)
  , _bgarrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchGetApplicationRevisionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgarrsApplicationName' - The name of the application that corresponds to the revisions.
--
-- * 'bgarrsRevisions' - Additional information about the revisions, including the type and location.
--
-- * 'bgarrsErrorMessage' - Information about errors that may have occurred during the API call.
--
-- * 'bgarrsResponseStatus' - -- | The response status code.
batchGetApplicationRevisionsResponse
    :: Int -- ^ 'bgarrsResponseStatus'
    -> BatchGetApplicationRevisionsResponse
batchGetApplicationRevisionsResponse pResponseStatus_ =
  BatchGetApplicationRevisionsResponse'
    { _bgarrsApplicationName = Nothing
    , _bgarrsRevisions = Nothing
    , _bgarrsErrorMessage = Nothing
    , _bgarrsResponseStatus = pResponseStatus_
    }


-- | The name of the application that corresponds to the revisions.
bgarrsApplicationName :: Lens' BatchGetApplicationRevisionsResponse (Maybe Text)
bgarrsApplicationName = lens _bgarrsApplicationName (\ s a -> s{_bgarrsApplicationName = a})

-- | Additional information about the revisions, including the type and location.
bgarrsRevisions :: Lens' BatchGetApplicationRevisionsResponse [RevisionInfo]
bgarrsRevisions = lens _bgarrsRevisions (\ s a -> s{_bgarrsRevisions = a}) . _Default . _Coerce

-- | Information about errors that may have occurred during the API call.
bgarrsErrorMessage :: Lens' BatchGetApplicationRevisionsResponse (Maybe Text)
bgarrsErrorMessage = lens _bgarrsErrorMessage (\ s a -> s{_bgarrsErrorMessage = a})

-- | -- | The response status code.
bgarrsResponseStatus :: Lens' BatchGetApplicationRevisionsResponse Int
bgarrsResponseStatus = lens _bgarrsResponseStatus (\ s a -> s{_bgarrsResponseStatus = a})

instance NFData BatchGetApplicationRevisionsResponse
         where
