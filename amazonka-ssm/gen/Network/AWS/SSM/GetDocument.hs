{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SSM.GetDocument
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
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

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.SSM.Types
import qualified GHC.Exts

newtype GetDocument = GetDocument
    { _gdName :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'GetDocument' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdName' @::@ 'Text'
--
getDocument :: Text -- ^ 'gdName'
            -> GetDocument
getDocument p1 = GetDocument
    { _gdName = p1
    }

-- | The name of the configuration document.
gdName :: Lens' GetDocument Text
gdName = lens _gdName (\s a -> s { _gdName = a })

data GetDocumentResponse = GetDocumentResponse
    { _gdrContent :: Maybe Text
    , _gdrName    :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'GetDocumentResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdrContent' @::@ 'Maybe' 'Text'
--
-- * 'gdrName' @::@ 'Maybe' 'Text'
--
getDocumentResponse :: GetDocumentResponse
getDocumentResponse = GetDocumentResponse
    { _gdrName    = Nothing
    , _gdrContent = Nothing
    }

-- | The contents of the configuration document.
gdrContent :: Lens' GetDocumentResponse (Maybe Text)
gdrContent = lens _gdrContent (\s a -> s { _gdrContent = a })

-- | The name of the configuration document.
gdrName :: Lens' GetDocumentResponse (Maybe Text)
gdrName = lens _gdrName (\s a -> s { _gdrName = a })

instance ToPath GetDocument where
    toPath = const "/"

instance ToQuery GetDocument where
    toQuery = const mempty

instance ToHeaders GetDocument

instance ToJSON GetDocument where
    toJSON GetDocument{..} = object
        [ "Name" .= _gdName
        ]

instance AWSRequest GetDocument where
    type Sv GetDocument = SSM
    type Rs GetDocument = GetDocumentResponse

    request  = post "GetDocument"
    response = jsonResponse

instance FromJSON GetDocumentResponse where
    parseJSON = withObject "GetDocumentResponse" $ \o -> GetDocumentResponse
        <$> o .:? "Content"
        <*> o .:? "Name"
