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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Systems Manager document.
--
--
-- After you create a document, you can use CreateAssociation to associate it with one or more running instances.
--
module Network.AWS.SSM.CreateDocument
    (
    -- * Creating a Request
      createDocument
    , CreateDocument
    -- * Request Lenses
    , cdDocumentType
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
    { _cdDocumentType :: !(Maybe DocumentType)
    , _cdContent      :: !Text
    , _cdName         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateDocument' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdDocumentType' - The type of document to create. Valid document types include: Policy, Automation, and Command.
--
-- * 'cdContent' - A valid JSON string.
--
-- * 'cdName' - A name for the Systems Manager document.
createDocument
    :: Text -- ^ 'cdContent'
    -> Text -- ^ 'cdName'
    -> CreateDocument
createDocument pContent_ pName_ =
    CreateDocument'
    { _cdDocumentType = Nothing
    , _cdContent = pContent_
    , _cdName = pName_
    }

-- | The type of document to create. Valid document types include: Policy, Automation, and Command.
cdDocumentType :: Lens' CreateDocument (Maybe DocumentType)
cdDocumentType = lens _cdDocumentType (\ s a -> s{_cdDocumentType = a});

-- | A valid JSON string.
cdContent :: Lens' CreateDocument Text
cdContent = lens _cdContent (\ s a -> s{_cdContent = a});

-- | A name for the Systems Manager document.
cdName :: Lens' CreateDocument Text
cdName = lens _cdName (\ s a -> s{_cdName = a});

instance AWSRequest CreateDocument where
        type Rs CreateDocument = CreateDocumentResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 CreateDocumentResponse' <$>
                   (x .?> "DocumentDescription") <*>
                     (pure (fromEnum s)))

instance Hashable CreateDocument

instance NFData CreateDocument

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
                 [("DocumentType" .=) <$> _cdDocumentType,
                  Just ("Content" .= _cdContent),
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
-- * 'cdrsDocumentDescription' - Information about the Systems Manager document.
--
-- * 'cdrsResponseStatus' - -- | The response status code.
createDocumentResponse
    :: Int -- ^ 'cdrsResponseStatus'
    -> CreateDocumentResponse
createDocumentResponse pResponseStatus_ =
    CreateDocumentResponse'
    { _cdrsDocumentDescription = Nothing
    , _cdrsResponseStatus = pResponseStatus_
    }

-- | Information about the Systems Manager document.
cdrsDocumentDescription :: Lens' CreateDocumentResponse (Maybe DocumentDescription)
cdrsDocumentDescription = lens _cdrsDocumentDescription (\ s a -> s{_cdrsDocumentDescription = a});

-- | -- | The response status code.
cdrsResponseStatus :: Lens' CreateDocumentResponse Int
cdrsResponseStatus = lens _cdrsResponseStatus (\ s a -> s{_cdrsResponseStatus = a});

instance NFData CreateDocumentResponse
