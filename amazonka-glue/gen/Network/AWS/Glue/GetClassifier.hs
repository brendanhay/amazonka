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
-- Module      : Network.AWS.Glue.GetClassifier
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a classifier by name.
--
--
module Network.AWS.Glue.GetClassifier
    (
    -- * Creating a Request
      getClassifier
    , GetClassifier
    -- * Request Lenses
    , getName

    -- * Destructuring the Response
    , getClassifierResponse
    , GetClassifierResponse
    -- * Response Lenses
    , gcrsClassifier
    , gcrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getClassifier' smart constructor.
newtype GetClassifier = GetClassifier'
  { _getName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetClassifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'getName' - Name of the classifier to retrieve.
getClassifier
    :: Text -- ^ 'getName'
    -> GetClassifier
getClassifier pName_ = GetClassifier' {_getName = pName_}


-- | Name of the classifier to retrieve.
getName :: Lens' GetClassifier Text
getName = lens _getName (\ s a -> s{_getName = a})

instance AWSRequest GetClassifier where
        type Rs GetClassifier = GetClassifierResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 GetClassifierResponse' <$>
                   (x .?> "Classifier") <*> (pure (fromEnum s)))

instance Hashable GetClassifier where

instance NFData GetClassifier where

instance ToHeaders GetClassifier where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.GetClassifier" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetClassifier where
        toJSON GetClassifier'{..}
          = object (catMaybes [Just ("Name" .= _getName)])

instance ToPath GetClassifier where
        toPath = const "/"

instance ToQuery GetClassifier where
        toQuery = const mempty

-- | /See:/ 'getClassifierResponse' smart constructor.
data GetClassifierResponse = GetClassifierResponse'
  { _gcrsClassifier     :: !(Maybe Classifier)
  , _gcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetClassifierResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcrsClassifier' - The requested classifier.
--
-- * 'gcrsResponseStatus' - -- | The response status code.
getClassifierResponse
    :: Int -- ^ 'gcrsResponseStatus'
    -> GetClassifierResponse
getClassifierResponse pResponseStatus_ =
  GetClassifierResponse'
    {_gcrsClassifier = Nothing, _gcrsResponseStatus = pResponseStatus_}


-- | The requested classifier.
gcrsClassifier :: Lens' GetClassifierResponse (Maybe Classifier)
gcrsClassifier = lens _gcrsClassifier (\ s a -> s{_gcrsClassifier = a})

-- | -- | The response status code.
gcrsResponseStatus :: Lens' GetClassifierResponse Int
gcrsResponseStatus = lens _gcrsResponseStatus (\ s a -> s{_gcrsResponseStatus = a})

instance NFData GetClassifierResponse where
