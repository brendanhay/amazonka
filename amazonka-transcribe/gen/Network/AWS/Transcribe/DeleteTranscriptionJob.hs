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
-- Module      : Network.AWS.Transcribe.DeleteTranscriptionJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a previously submitted transcription job along with any other generated results such as the transcription, models, and so on.
--
--
module Network.AWS.Transcribe.DeleteTranscriptionJob
    (
    -- * Creating a Request
      deleteTranscriptionJob
    , DeleteTranscriptionJob
    -- * Request Lenses
    , dtjTranscriptionJobName

    -- * Destructuring the Response
    , deleteTranscriptionJobResponse
    , DeleteTranscriptionJobResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Transcribe.Types
import Network.AWS.Transcribe.Types.Product

-- | /See:/ 'deleteTranscriptionJob' smart constructor.
newtype DeleteTranscriptionJob = DeleteTranscriptionJob'
  { _dtjTranscriptionJobName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTranscriptionJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtjTranscriptionJobName' - The name of the transcription job to be deleted.
deleteTranscriptionJob
    :: Text -- ^ 'dtjTranscriptionJobName'
    -> DeleteTranscriptionJob
deleteTranscriptionJob pTranscriptionJobName_ =
  DeleteTranscriptionJob' {_dtjTranscriptionJobName = pTranscriptionJobName_}


-- | The name of the transcription job to be deleted.
dtjTranscriptionJobName :: Lens' DeleteTranscriptionJob Text
dtjTranscriptionJobName = lens _dtjTranscriptionJobName (\ s a -> s{_dtjTranscriptionJobName = a})

instance AWSRequest DeleteTranscriptionJob where
        type Rs DeleteTranscriptionJob =
             DeleteTranscriptionJobResponse
        request = postJSON transcribe
        response
          = receiveNull DeleteTranscriptionJobResponse'

instance Hashable DeleteTranscriptionJob where

instance NFData DeleteTranscriptionJob where

instance ToHeaders DeleteTranscriptionJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Transcribe.DeleteTranscriptionJob" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteTranscriptionJob where
        toJSON DeleteTranscriptionJob'{..}
          = object
              (catMaybes
                 [Just
                    ("TranscriptionJobName" .=
                       _dtjTranscriptionJobName)])

instance ToPath DeleteTranscriptionJob where
        toPath = const "/"

instance ToQuery DeleteTranscriptionJob where
        toQuery = const mempty

-- | /See:/ 'deleteTranscriptionJobResponse' smart constructor.
data DeleteTranscriptionJobResponse =
  DeleteTranscriptionJobResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTranscriptionJobResponse' with the minimum fields required to make a request.
--
deleteTranscriptionJobResponse
    :: DeleteTranscriptionJobResponse
deleteTranscriptionJobResponse = DeleteTranscriptionJobResponse'


instance NFData DeleteTranscriptionJobResponse where
