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
-- Module      : Network.AWS.SSM.CancelCommand
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to cancel the command specified by the Command ID. There is no guarantee that the command will be terminated and the underlying process stopped.
--
--
module Network.AWS.SSM.CancelCommand
    (
    -- * Creating a Request
      cancelCommand
    , CancelCommand
    -- * Request Lenses
    , ccInstanceIds
    , ccCommandId

    -- * Destructuring the Response
    , cancelCommandResponse
    , CancelCommandResponse
    -- * Response Lenses
    , ccrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- |
--
--
--
-- /See:/ 'cancelCommand' smart constructor.
data CancelCommand = CancelCommand'
  { _ccInstanceIds :: !(Maybe [Text])
  , _ccCommandId   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelCommand' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccInstanceIds' - (Optional) A list of instance IDs on which you want to cancel the command. If not provided, the command is canceled on every instance on which it was requested.
--
-- * 'ccCommandId' - The ID of the command you want to cancel.
cancelCommand
    :: Text -- ^ 'ccCommandId'
    -> CancelCommand
cancelCommand pCommandId_ =
  CancelCommand' {_ccInstanceIds = Nothing, _ccCommandId = pCommandId_}


-- | (Optional) A list of instance IDs on which you want to cancel the command. If not provided, the command is canceled on every instance on which it was requested.
ccInstanceIds :: Lens' CancelCommand [Text]
ccInstanceIds = lens _ccInstanceIds (\ s a -> s{_ccInstanceIds = a}) . _Default . _Coerce

-- | The ID of the command you want to cancel.
ccCommandId :: Lens' CancelCommand Text
ccCommandId = lens _ccCommandId (\ s a -> s{_ccCommandId = a})

instance AWSRequest CancelCommand where
        type Rs CancelCommand = CancelCommandResponse
        request = postJSON ssm
        response
          = receiveEmpty
              (\ s h x ->
                 CancelCommandResponse' <$> (pure (fromEnum s)))

instance Hashable CancelCommand where

instance NFData CancelCommand where

instance ToHeaders CancelCommand where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.CancelCommand" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CancelCommand where
        toJSON CancelCommand'{..}
          = object
              (catMaybes
                 [("InstanceIds" .=) <$> _ccInstanceIds,
                  Just ("CommandId" .= _ccCommandId)])

instance ToPath CancelCommand where
        toPath = const "/"

instance ToQuery CancelCommand where
        toQuery = const mempty

-- | Whether or not the command was successfully canceled. There is no guarantee that a request can be canceled.
--
--
--
-- /See:/ 'cancelCommandResponse' smart constructor.
newtype CancelCommandResponse = CancelCommandResponse'
  { _ccrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelCommandResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccrsResponseStatus' - -- | The response status code.
cancelCommandResponse
    :: Int -- ^ 'ccrsResponseStatus'
    -> CancelCommandResponse
cancelCommandResponse pResponseStatus_ =
  CancelCommandResponse' {_ccrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ccrsResponseStatus :: Lens' CancelCommandResponse Int
ccrsResponseStatus = lens _ccrsResponseStatus (\ s a -> s{_ccrsResponseStatus = a})

instance NFData CancelCommandResponse where
