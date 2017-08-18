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
-- Module      : Network.AWS.Glue.UpdateClassifier
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing @Classifier@ .
--
--
module Network.AWS.Glue.UpdateClassifier
    (
    -- * Creating a Request
      updateClassifier
    , UpdateClassifier
    -- * Request Lenses
    , ucGrokClassifier

    -- * Destructuring the Response
    , updateClassifierResponse
    , UpdateClassifierResponse
    -- * Response Lenses
    , ursResponseStatus
    ) where

import           Network.AWS.Glue.Types
import           Network.AWS.Glue.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateClassifier' smart constructor.
newtype UpdateClassifier = UpdateClassifier'
    { _ucGrokClassifier :: Maybe UpdateGrokClassifierRequest
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateClassifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucGrokClassifier' - A @GrokClassifier@ object with updated fields.
updateClassifier
    :: UpdateClassifier
updateClassifier =
    UpdateClassifier'
    { _ucGrokClassifier = Nothing
    }

-- | A @GrokClassifier@ object with updated fields.
ucGrokClassifier :: Lens' UpdateClassifier (Maybe UpdateGrokClassifierRequest)
ucGrokClassifier = lens _ucGrokClassifier (\ s a -> s{_ucGrokClassifier = a});

instance AWSRequest UpdateClassifier where
        type Rs UpdateClassifier = UpdateClassifierResponse
        request = postJSON glue
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateClassifierResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateClassifier

instance NFData UpdateClassifier

instance ToHeaders UpdateClassifier where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.UpdateClassifier" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateClassifier where
        toJSON UpdateClassifier'{..}
          = object
              (catMaybes
                 [("GrokClassifier" .=) <$> _ucGrokClassifier])

instance ToPath UpdateClassifier where
        toPath = const "/"

instance ToQuery UpdateClassifier where
        toQuery = const mempty

-- | /See:/ 'updateClassifierResponse' smart constructor.
newtype UpdateClassifierResponse = UpdateClassifierResponse'
    { _ursResponseStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateClassifierResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ursResponseStatus' - -- | The response status code.
updateClassifierResponse
    :: Int -- ^ 'ursResponseStatus'
    -> UpdateClassifierResponse
updateClassifierResponse pResponseStatus_ =
    UpdateClassifierResponse'
    { _ursResponseStatus = pResponseStatus_
    }

-- | -- | The response status code.
ursResponseStatus :: Lens' UpdateClassifierResponse Int
ursResponseStatus = lens _ursResponseStatus (\ s a -> s{_ursResponseStatus = a});

instance NFData UpdateClassifierResponse
