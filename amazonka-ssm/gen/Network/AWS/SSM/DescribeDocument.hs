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

-- Module      : Network.AWS.SSM.DescribeDocument
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

-- | Describes the specified configuration document.
--
-- <http://docs.aws.amazon.com/ssm/latest/APIReference/API_DescribeDocument.html>
module Network.AWS.SSM.DescribeDocument
    (
    -- * Request
      DescribeDocument
    -- ** Request constructor
    , describeDocument
    -- ** Request lenses
    , ddName

    -- * Response
    , DescribeDocumentResponse
    -- ** Response constructor
    , describeDocumentResponse
    -- ** Response lenses
    , ddrDocument
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.SSM.Types
import qualified GHC.Exts

newtype DescribeDocument = DescribeDocument
    { _ddName :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'DescribeDocument' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddName' @::@ 'Text'
--
describeDocument :: Text -- ^ 'ddName'
                 -> DescribeDocument
describeDocument p1 = DescribeDocument
    { _ddName = p1
    }

-- | The name of the configuration document.
ddName :: Lens' DescribeDocument Text
ddName = lens _ddName (\s a -> s { _ddName = a })

newtype DescribeDocumentResponse = DescribeDocumentResponse
    { _ddrDocument :: Maybe DocumentDescription
    } deriving (Eq, Read, Show)

-- | 'DescribeDocumentResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddrDocument' @::@ 'Maybe' 'DocumentDescription'
--
describeDocumentResponse :: DescribeDocumentResponse
describeDocumentResponse = DescribeDocumentResponse
    { _ddrDocument = Nothing
    }

-- | Information about the configuration document.
ddrDocument :: Lens' DescribeDocumentResponse (Maybe DocumentDescription)
ddrDocument = lens _ddrDocument (\s a -> s { _ddrDocument = a })

instance ToPath DescribeDocument where
    toPath = const "/"

instance ToQuery DescribeDocument where
    toQuery = const mempty

instance ToHeaders DescribeDocument

instance ToJSON DescribeDocument where
    toJSON DescribeDocument{..} = object
        [ "Name" .= _ddName
        ]

instance AWSRequest DescribeDocument where
    type Sv DescribeDocument = SSM
    type Rs DescribeDocument = DescribeDocumentResponse

    request  = post "DescribeDocument"
    response = jsonResponse

instance FromJSON DescribeDocumentResponse where
    parseJSON = withObject "DescribeDocumentResponse" $ \o -> DescribeDocumentResponse
        <$> o .:? "Document"
