{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DeleteDocument
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified configuration document.
--
-- You must use DeleteAssociation to disassociate all instances that are
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
    , dddName

    -- * Response
    , DeleteDocumentResponse
    -- ** Response constructor
    , deleteDocumentResponse
    -- ** Response lenses
    , ddrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SSM.Types

-- | /See:/ 'deleteDocument' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dddName'
newtype DeleteDocument = DeleteDocument'
    { _dddName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDocument' smart constructor.
deleteDocument :: Text -> DeleteDocument
deleteDocument pName_ =
    DeleteDocument'
    { _dddName = pName_
    }

-- | The name of the configuration document.
dddName :: Lens' DeleteDocument Text
dddName = lens _dddName (\ s a -> s{_dddName = a});

instance AWSRequest DeleteDocument where
        type Sv DeleteDocument = SSM
        type Rs DeleteDocument = DeleteDocumentResponse
        request = postJSON "DeleteDocument"
        response
          = receiveJSON
              (\ s h x ->
                 DeleteDocumentResponse' <$> (pure (fromEnum s)))

instance ToHeaders DeleteDocument where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DeleteDocument" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteDocument where
        toJSON DeleteDocument'{..}
          = object ["Name" .= _dddName]

instance ToPath DeleteDocument where
        toPath = const "/"

instance ToQuery DeleteDocument where
        toQuery = const mempty

-- | /See:/ 'deleteDocumentResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddrsStatus'
newtype DeleteDocumentResponse = DeleteDocumentResponse'
    { _ddrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDocumentResponse' smart constructor.
deleteDocumentResponse :: Int -> DeleteDocumentResponse
deleteDocumentResponse pStatus_ =
    DeleteDocumentResponse'
    { _ddrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
ddrsStatus :: Lens' DeleteDocumentResponse Int
ddrsStatus = lens _ddrsStatus (\ s a -> s{_ddrsStatus = a});
