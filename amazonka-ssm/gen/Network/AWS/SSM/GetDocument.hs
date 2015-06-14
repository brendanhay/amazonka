{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.SSM.GetDocument
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

-- | Gets the contents of the specified configuration document.
--
-- <http://docs.aws.amazon.com/ssm/latest/APIReference/API_GetDocument.html>
module Network.AWS.SSM.GetDocument
    (
    -- * Request
      GetDocument
    -- ** Request constructor
    , getDocument
    -- ** Request lenses
    , gdName

    -- * Response
    , GetDocumentResponse
    -- ** Response constructor
    , getDocumentResponse
    -- ** Response lenses
    , gdrContent
    , gdrName
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.SSM.Types

-- | /See:/ 'getDocument' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdName'
newtype GetDocument = GetDocument'{_gdName :: Text} deriving (Eq, Read, Show)

-- | 'GetDocument' smart constructor.
getDocument :: Text -> GetDocument
getDocument pName = GetDocument'{_gdName = pName};

-- | The name of the configuration document.
gdName :: Lens' GetDocument Text
gdName = lens _gdName (\ s a -> s{_gdName = a});

instance AWSRequest GetDocument where
        type Sv GetDocument = SSM
        type Rs GetDocument = GetDocumentResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetDocumentResponse' <$>
                   x .:> "Content" <*> x .:> "Name")

instance ToHeaders GetDocument where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.GetDocument" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDocument where
        toJSON GetDocument'{..} = object ["Name" .= _gdName]

instance ToPath GetDocument where
        toPath = const "/"

instance ToQuery GetDocument where
        toQuery = const mempty

-- | /See:/ 'getDocumentResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdrContent'
--
-- * 'gdrName'
data GetDocumentResponse = GetDocumentResponse'{_gdrContent :: Text, _gdrName :: Text} deriving (Eq, Read, Show)

-- | 'GetDocumentResponse' smart constructor.
getDocumentResponse :: Text -> Text -> GetDocumentResponse
getDocumentResponse pContent pName = GetDocumentResponse'{_gdrContent = pContent, _gdrName = pName};

-- | The contents of the configuration document.
gdrContent :: Lens' GetDocumentResponse Text
gdrContent = lens _gdrContent (\ s a -> s{_gdrContent = a});

-- | The name of the configuration document.
gdrName :: Lens' GetDocumentResponse Text
gdrName = lens _gdrName (\ s a -> s{_gdrName = a});
