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

-- Module      : Network.AWS.SSM.DeleteDocument
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

-- | Deletes the specified configuration document.
--
-- You must use 'DeleteAssociation' to disassociate all instances that are
-- associated with the configuration document before you can delete it.
--
-- <http://docs.aws.amazon.com/ssm/latest/APIReference/API_DeleteDocument.html>
module Network.AWS.SSM.DeleteDocument
    (
    -- * Request
      DeleteDocument
    -- ** Request constructor
    , deleteDocument
    -- ** Request lenses
    , dd2Name

    -- * Response
    , DeleteDocumentResponse
    -- ** Response constructor
    , deleteDocumentResponse
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.SSM.Types
import qualified GHC.Exts

newtype DeleteDocument = DeleteDocument
    { _dd2Name :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'DeleteDocument' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dd2Name' @::@ 'Text'
--
deleteDocument :: Text -- ^ 'dd2Name'
               -> DeleteDocument
deleteDocument p1 = DeleteDocument
    { _dd2Name = p1
    }

-- | The name of the configuration document.
dd2Name :: Lens' DeleteDocument Text
dd2Name = lens _dd2Name (\s a -> s { _dd2Name = a })

data DeleteDocumentResponse = DeleteDocumentResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeleteDocumentResponse' constructor.
deleteDocumentResponse :: DeleteDocumentResponse
deleteDocumentResponse = DeleteDocumentResponse

instance ToPath DeleteDocument where
    toPath = const "/"

instance ToQuery DeleteDocument where
    toQuery = const mempty

instance ToHeaders DeleteDocument

instance ToJSON DeleteDocument where
    toJSON DeleteDocument{..} = object
        [ "Name" .= _dd2Name
        ]

instance AWSRequest DeleteDocument where
    type Sv DeleteDocument = SSM
    type Rs DeleteDocument = DeleteDocumentResponse

    request  = post "DeleteDocument"
    response = nullResponse DeleteDocumentResponse
