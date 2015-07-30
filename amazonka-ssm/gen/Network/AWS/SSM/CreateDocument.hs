{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.CreateDocument
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a configuration document.
--
-- After you create a configuration document, you can use CreateAssociation
-- to associate it with one or more running instances.
--
-- <http://docs.aws.amazon.com/ssm/latest/APIReference/API_CreateDocument.html>
module Network.AWS.SSM.CreateDocument
    (
    -- * Request
      CreateDocument
    -- ** Request constructor
    , createDocument
    -- ** Request lenses
    , cdContent
    , cdName

    -- * Response
    , CreateDocumentResponse
    -- ** Response constructor
    , createDocumentResponse
    -- ** Response lenses
    , cdrsDocumentDescription
    , cdrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SSM.Types

-- | /See:/ 'createDocument' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdContent'
--
-- * 'cdName'
data CreateDocument = CreateDocument'
    { _cdContent :: !Text
    , _cdName    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDocument' smart constructor.
createDocument :: Text -> Text -> CreateDocument
createDocument pContent_ pName_ =
    CreateDocument'
    { _cdContent = pContent_
    , _cdName = pName_
    }

-- | A valid JSON file. For more information about the contents of this file,
-- see
-- <http://docs.aws.amazon.com/ssm/latest/APIReference/aws-ssm-document.html Configuration Document>.
cdContent :: Lens' CreateDocument Text
cdContent = lens _cdContent (\ s a -> s{_cdContent = a});

-- | A name for the configuration document.
cdName :: Lens' CreateDocument Text
cdName = lens _cdName (\ s a -> s{_cdName = a});

instance AWSRequest CreateDocument where
        type Sv CreateDocument = SSM
        type Rs CreateDocument = CreateDocumentResponse
        request = postJSON
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
          = object ["Content" .= _cdContent, "Name" .= _cdName]

instance ToPath CreateDocument where
        toPath = const mempty

instance ToQuery CreateDocument where
        toQuery = const mempty

-- | /See:/ 'createDocumentResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdrsDocumentDescription'
--
-- * 'cdrsStatus'
data CreateDocumentResponse = CreateDocumentResponse'
    { _cdrsDocumentDescription :: !(Maybe DocumentDescription)
    , _cdrsStatus              :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDocumentResponse' smart constructor.
createDocumentResponse :: Int -> CreateDocumentResponse
createDocumentResponse pStatus_ =
    CreateDocumentResponse'
    { _cdrsDocumentDescription = Nothing
    , _cdrsStatus = pStatus_
    }

-- | Information about the configuration document.
cdrsDocumentDescription :: Lens' CreateDocumentResponse (Maybe DocumentDescription)
cdrsDocumentDescription = lens _cdrsDocumentDescription (\ s a -> s{_cdrsDocumentDescription = a});

-- | FIXME: Undocumented member.
cdrsStatus :: Lens' CreateDocumentResponse Int
cdrsStatus = lens _cdrsStatus (\ s a -> s{_cdrsStatus = a});
