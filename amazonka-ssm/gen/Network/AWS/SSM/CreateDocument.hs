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
-- Module      : Network.AWS.SSM.CreateDocument
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an SSM document.
--
-- After you create an SSM document, you can use CreateAssociation to
-- associate it with one or more running instances.
--
-- /See:/ <http://docs.aws.amazon.com/ssm/latest/APIReference/API_CreateDocument.html AWS API Reference> for CreateDocument.
module Network.AWS.SSM.CreateDocument
    (
    -- * Creating a Request
      createDocument
    , CreateDocument
    -- * Request Lenses
    , cdContent
    , cdName

    -- * Destructuring the Response
    , createDocumentResponse
    , CreateDocumentResponse
    -- * Response Lenses
    , cdrsDocumentDescription
    , cdrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SSM.Types
import           Network.AWS.SSM.Types.Product

-- | /See:/ 'createDocument' smart constructor.
data CreateDocument = CreateDocument'
    { _cdContent :: !Text
    , _cdName    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateDocument' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdContent'
--
-- * 'cdName'
createDocument
    :: Text -- ^ 'cdContent'
    -> Text -- ^ 'cdName'
    -> CreateDocument
createDocument pContent_ pName_ =
    CreateDocument'
    { _cdContent = pContent_
    , _cdName = pName_
    }

-- | A valid JSON string. For more information about the contents of this
-- string, see
-- <http://docs.aws.amazon.com/ssm/latest/APIReference/aws-ssm-document.html SSM Document>.
cdContent :: Lens' CreateDocument Text
cdContent = lens _cdContent (\ s a -> s{_cdContent = a});

-- | A name for the SSM document.
cdName :: Lens' CreateDocument Text
cdName = lens _cdName (\ s a -> s{_cdName = a});

instance AWSRequest CreateDocument where
        type Rs CreateDocument = CreateDocumentResponse
        request = postJSON sSM
        response
          = receiveJSON
              (\ s h x ->
                 CreateDocumentResponse' <$>
                   (x .?> "DocumentDescription") <*>
                     (pure (fromEnum s)))

instance ToHeaders CreateDocument where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.CreateDocument" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateDocument where
        toJSON CreateDocument'{..}
          = object
              (catMaybes
                 [Just ("Content" .= _cdContent),
                  Just ("Name" .= _cdName)])

instance ToPath CreateDocument where
        toPath = const "/"

instance ToQuery CreateDocument where
        toQuery = const mempty

-- | /See:/ 'createDocumentResponse' smart constructor.
data CreateDocumentResponse = CreateDocumentResponse'
    { _cdrsDocumentDescription :: !(Maybe DocumentDescription)
    , _cdrsResponseStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateDocumentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdrsDocumentDescription'
--
-- * 'cdrsResponseStatus'
createDocumentResponse
    :: Int -- ^ 'cdrsResponseStatus'
    -> CreateDocumentResponse
createDocumentResponse pResponseStatus_ =
    CreateDocumentResponse'
    { _cdrsDocumentDescription = Nothing
    , _cdrsResponseStatus = pResponseStatus_
    }

-- | Information about the SSM document.
cdrsDocumentDescription :: Lens' CreateDocumentResponse (Maybe DocumentDescription)
cdrsDocumentDescription = lens _cdrsDocumentDescription (\ s a -> s{_cdrsDocumentDescription = a});

-- | The response status code.
cdrsResponseStatus :: Lens' CreateDocumentResponse Int
cdrsResponseStatus = lens _cdrsResponseStatus (\ s a -> s{_cdrsResponseStatus = a});
