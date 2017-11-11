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
-- Module      : Network.AWS.Glue.CreateClassifier
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @Classifier@ in the user's account.
--
--
module Network.AWS.Glue.CreateClassifier
    (
    -- * Creating a Request
      createClassifier
    , CreateClassifier
    -- * Request Lenses
    , ccGrokClassifier

    -- * Destructuring the Response
    , createClassifierResponse
    , CreateClassifierResponse
    -- * Response Lenses
    , ccrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createClassifier' smart constructor.
newtype CreateClassifier = CreateClassifier'
  { _ccGrokClassifier :: Maybe CreateGrokClassifierRequest
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateClassifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccGrokClassifier' - A grok classifier to create.
createClassifier
    :: CreateClassifier
createClassifier = CreateClassifier' {_ccGrokClassifier = Nothing}


-- | A grok classifier to create.
ccGrokClassifier :: Lens' CreateClassifier (Maybe CreateGrokClassifierRequest)
ccGrokClassifier = lens _ccGrokClassifier (\ s a -> s{_ccGrokClassifier = a});

instance AWSRequest CreateClassifier where
        type Rs CreateClassifier = CreateClassifierResponse
        request = postJSON glue
        response
          = receiveEmpty
              (\ s h x ->
                 CreateClassifierResponse' <$> (pure (fromEnum s)))

instance Hashable CreateClassifier where

instance NFData CreateClassifier where

instance ToHeaders CreateClassifier where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.CreateClassifier" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateClassifier where
        toJSON CreateClassifier'{..}
          = object
              (catMaybes
                 [("GrokClassifier" .=) <$> _ccGrokClassifier])

instance ToPath CreateClassifier where
        toPath = const "/"

instance ToQuery CreateClassifier where
        toQuery = const mempty

-- | /See:/ 'createClassifierResponse' smart constructor.
newtype CreateClassifierResponse = CreateClassifierResponse'
  { _ccrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateClassifierResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccrsResponseStatus' - -- | The response status code.
createClassifierResponse
    :: Int -- ^ 'ccrsResponseStatus'
    -> CreateClassifierResponse
createClassifierResponse pResponseStatus_ =
  CreateClassifierResponse' {_ccrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ccrsResponseStatus :: Lens' CreateClassifierResponse Int
ccrsResponseStatus = lens _ccrsResponseStatus (\ s a -> s{_ccrsResponseStatus = a});

instance NFData CreateClassifierResponse where
