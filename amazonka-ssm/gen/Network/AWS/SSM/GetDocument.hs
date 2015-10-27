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
-- Module      : Network.AWS.SSM.GetDocument
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the contents of the specified SSM document.
--
-- /See:/ <http://docs.aws.amazon.com/ssm/latest/APIReference/API_GetDocument.html AWS API Reference> for GetDocument.
module Network.AWS.SSM.GetDocument
    (
    -- * Creating a Request
      getDocument
    , GetDocument
    -- * Request Lenses
    , gdName

    -- * Destructuring the Response
    , getDocumentResponse
    , GetDocumentResponse
    -- * Response Lenses
    , gdrsContent
    , gdrsName
    , gdrsResponseStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SSM.Types
import           Network.AWS.SSM.Types.Product

-- | /See:/ 'getDocument' smart constructor.
newtype GetDocument = GetDocument'
    { _gdName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetDocument' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdName'
getDocument
    :: Text -- ^ 'gdName'
    -> GetDocument
getDocument pName_ =
    GetDocument'
    { _gdName = pName_
    }

-- | The name of the SSM document.
gdName :: Lens' GetDocument Text
gdName = lens _gdName (\ s a -> s{_gdName = a});

instance AWSRequest GetDocument where
        type Rs GetDocument = GetDocumentResponse
        request = postJSON sSM
        response
          = receiveJSON
              (\ s h x ->
                 GetDocumentResponse' <$>
                   (x .?> "Content") <*> (x .?> "Name") <*>
                     (pure (fromEnum s)))

instance ToHeaders GetDocument where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.GetDocument" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDocument where
        toJSON GetDocument'{..}
          = object (catMaybes [Just ("Name" .= _gdName)])

instance ToPath GetDocument where
        toPath = const "/"

instance ToQuery GetDocument where
        toQuery = const mempty

-- | /See:/ 'getDocumentResponse' smart constructor.
data GetDocumentResponse = GetDocumentResponse'
    { _gdrsContent        :: !(Maybe Text)
    , _gdrsName           :: !(Maybe Text)
    , _gdrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetDocumentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdrsContent'
--
-- * 'gdrsName'
--
-- * 'gdrsResponseStatus'
getDocumentResponse
    :: Int -- ^ 'gdrsResponseStatus'
    -> GetDocumentResponse
getDocumentResponse pResponseStatus_ =
    GetDocumentResponse'
    { _gdrsContent = Nothing
    , _gdrsName = Nothing
    , _gdrsResponseStatus = pResponseStatus_
    }

-- | The contents of the SSM document.
gdrsContent :: Lens' GetDocumentResponse (Maybe Text)
gdrsContent = lens _gdrsContent (\ s a -> s{_gdrsContent = a});

-- | The name of the SSM document.
gdrsName :: Lens' GetDocumentResponse (Maybe Text)
gdrsName = lens _gdrsName (\ s a -> s{_gdrsName = a});

-- | The response status code.
gdrsResponseStatus :: Lens' GetDocumentResponse Int
gdrsResponseStatus = lens _gdrsResponseStatus (\ s a -> s{_gdrsResponseStatus = a});
