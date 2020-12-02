{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DeleteConferenceProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a conference provider.
module Network.AWS.AlexaBusiness.DeleteConferenceProvider
  ( -- * Creating a Request
    deleteConferenceProvider,
    DeleteConferenceProvider,

    -- * Request Lenses
    dcpConferenceProviderARN,

    -- * Destructuring the Response
    deleteConferenceProviderResponse,
    DeleteConferenceProviderResponse,

    -- * Response Lenses
    dcprsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteConferenceProvider' smart constructor.
newtype DeleteConferenceProvider = DeleteConferenceProvider'
  { _dcpConferenceProviderARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteConferenceProvider' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcpConferenceProviderARN' - The ARN of the conference provider.
deleteConferenceProvider ::
  -- | 'dcpConferenceProviderARN'
  Text ->
  DeleteConferenceProvider
deleteConferenceProvider pConferenceProviderARN_ =
  DeleteConferenceProvider'
    { _dcpConferenceProviderARN =
        pConferenceProviderARN_
    }

-- | The ARN of the conference provider.
dcpConferenceProviderARN :: Lens' DeleteConferenceProvider Text
dcpConferenceProviderARN = lens _dcpConferenceProviderARN (\s a -> s {_dcpConferenceProviderARN = a})

instance AWSRequest DeleteConferenceProvider where
  type Rs DeleteConferenceProvider = DeleteConferenceProviderResponse
  request = postJSON alexaBusiness
  response =
    receiveEmpty
      ( \s h x ->
          DeleteConferenceProviderResponse' <$> (pure (fromEnum s))
      )

instance Hashable DeleteConferenceProvider

instance NFData DeleteConferenceProvider

instance ToHeaders DeleteConferenceProvider where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AlexaForBusiness.DeleteConferenceProvider" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteConferenceProvider where
  toJSON DeleteConferenceProvider' {..} =
    object
      ( catMaybes
          [Just ("ConferenceProviderArn" .= _dcpConferenceProviderARN)]
      )

instance ToPath DeleteConferenceProvider where
  toPath = const "/"

instance ToQuery DeleteConferenceProvider where
  toQuery = const mempty

-- | /See:/ 'deleteConferenceProviderResponse' smart constructor.
newtype DeleteConferenceProviderResponse = DeleteConferenceProviderResponse'
  { _dcprsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteConferenceProviderResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcprsResponseStatus' - -- | The response status code.
deleteConferenceProviderResponse ::
  -- | 'dcprsResponseStatus'
  Int ->
  DeleteConferenceProviderResponse
deleteConferenceProviderResponse pResponseStatus_ =
  DeleteConferenceProviderResponse'
    { _dcprsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dcprsResponseStatus :: Lens' DeleteConferenceProviderResponse Int
dcprsResponseStatus = lens _dcprsResponseStatus (\s a -> s {_dcprsResponseStatus = a})

instance NFData DeleteConferenceProviderResponse
