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
    , cdrqContent
    , cdrqName

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
-- * 'cdrqContent'
--
-- * 'cdrqName'
data CreateDocument = CreateDocument'
    { _cdrqContent :: !Text
    , _cdrqName    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDocument' smart constructor.
createDocument :: Text -> Text -> CreateDocument
createDocument pContent pName =
    CreateDocument'
    { _cdrqContent = pContent
    , _cdrqName = pName
    }

-- | A valid JSON file. For more information about the contents of this file,
-- see
-- <http://docs.aws.amazon.com/ssm/latest/APIReference/aws-ssm-document.html Configuration Document>.
cdrqContent :: Lens' CreateDocument Text
cdrqContent = lens _cdrqContent (\ s a -> s{_cdrqContent = a});

-- | A name for the configuration document.
cdrqName :: Lens' CreateDocument Text
cdrqName = lens _cdrqName (\ s a -> s{_cdrqName = a});

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
          = object
              ["Content" .= _cdrqContent, "Name" .= _cdrqName]

instance ToPath CreateDocument where
        toPath = const "/"

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
createDocumentResponse pStatus =
    CreateDocumentResponse'
    { _cdrsDocumentDescription = Nothing
    , _cdrsStatus = pStatus
    }

-- | Information about the configuration document.
cdrsDocumentDescription :: Lens' CreateDocumentResponse (Maybe DocumentDescription)
cdrsDocumentDescription = lens _cdrsDocumentDescription (\ s a -> s{_cdrsDocumentDescription = a});

-- | FIXME: Undocumented member.
cdrsStatus :: Lens' CreateDocumentResponse Int
cdrsStatus = lens _cdrsStatus (\ s a -> s{_cdrsStatus = a});
