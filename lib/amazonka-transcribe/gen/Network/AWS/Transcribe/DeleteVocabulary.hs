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
-- Module      : Network.AWS.Transcribe.DeleteVocabulary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a vocabulary from Amazon Transcribe.
--
--
module Network.AWS.Transcribe.DeleteVocabulary
    (
    -- * Creating a Request
      deleteVocabulary
    , DeleteVocabulary
    -- * Request Lenses
    , dvVocabularyName

    -- * Destructuring the Response
    , deleteVocabularyResponse
    , DeleteVocabularyResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Transcribe.Types
import Network.AWS.Transcribe.Types.Product

-- | /See:/ 'deleteVocabulary' smart constructor.
newtype DeleteVocabulary = DeleteVocabulary'
  { _dvVocabularyName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVocabulary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvVocabularyName' - The name of the vocabulary to delete.
deleteVocabulary
    :: Text -- ^ 'dvVocabularyName'
    -> DeleteVocabulary
deleteVocabulary pVocabularyName_ =
  DeleteVocabulary' {_dvVocabularyName = pVocabularyName_}


-- | The name of the vocabulary to delete.
dvVocabularyName :: Lens' DeleteVocabulary Text
dvVocabularyName = lens _dvVocabularyName (\ s a -> s{_dvVocabularyName = a})

instance AWSRequest DeleteVocabulary where
        type Rs DeleteVocabulary = DeleteVocabularyResponse
        request = postJSON transcribe
        response = receiveNull DeleteVocabularyResponse'

instance Hashable DeleteVocabulary where

instance NFData DeleteVocabulary where

instance ToHeaders DeleteVocabulary where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Transcribe.DeleteVocabulary" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteVocabulary where
        toJSON DeleteVocabulary'{..}
          = object
              (catMaybes
                 [Just ("VocabularyName" .= _dvVocabularyName)])

instance ToPath DeleteVocabulary where
        toPath = const "/"

instance ToQuery DeleteVocabulary where
        toQuery = const mempty

-- | /See:/ 'deleteVocabularyResponse' smart constructor.
data DeleteVocabularyResponse =
  DeleteVocabularyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVocabularyResponse' with the minimum fields required to make a request.
--
deleteVocabularyResponse
    :: DeleteVocabularyResponse
deleteVocabularyResponse = DeleteVocabularyResponse'


instance NFData DeleteVocabularyResponse where
