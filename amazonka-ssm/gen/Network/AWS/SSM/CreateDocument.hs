{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.SSM.CreateDocument
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a configuration document.
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
    , cdrDocumentDescription
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types

-- | /See:/ 'createDocument' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdContent'
--
-- * 'cdName'
data CreateDocument = CreateDocument'{_cdContent :: Text, _cdName :: Text} deriving (Eq, Read, Show)

-- | 'CreateDocument' smart constructor.
createDocument :: Text -> Text -> CreateDocument
createDocument pContent pName = CreateDocument'{_cdContent = pContent, _cdName = pName};

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
                   (x .?> "DocumentDescription"))

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
        toPath = const "/"

instance ToQuery CreateDocument where
        toQuery = const mempty

-- | /See:/ 'createDocumentResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdrDocumentDescription'
newtype CreateDocumentResponse = CreateDocumentResponse'{_cdrDocumentDescription :: Maybe DocumentDescription} deriving (Eq, Read, Show)

-- | 'CreateDocumentResponse' smart constructor.
createDocumentResponse :: CreateDocumentResponse
createDocumentResponse = CreateDocumentResponse'{_cdrDocumentDescription = Nothing};

-- | Information about the configuration document.
cdrDocumentDescription :: Lens' CreateDocumentResponse (Maybe DocumentDescription)
cdrDocumentDescription = lens _cdrDocumentDescription (\ s a -> s{_cdrDocumentDescription = a});
