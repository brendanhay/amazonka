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
-- Module      : Network.AWS.Inspector.UpdateApplication
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates application specified by the application ARN.
--
-- /See:/ <http://docs.aws.amazon.com/inspector/latest/APIReference/API_UpdateApplication.html AWS API Reference> for UpdateApplication.
module Network.AWS.Inspector.UpdateApplication
    (
    -- * Creating a Request
      updateApplication
    , UpdateApplication
    -- * Request Lenses
    , uaApplicationARN
    , uaResourceGroupARN
    , uaApplicationName

    -- * Destructuring the Response
    , updateApplicationResponse
    , UpdateApplicationResponse
    -- * Response Lenses
    , uarsMessage
    , uarsResponseStatus
    ) where

import           Network.AWS.Inspector.Types
import           Network.AWS.Inspector.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateApplication' smart constructor.
data UpdateApplication = UpdateApplication'
    { _uaApplicationARN   :: !(Maybe Text)
    , _uaResourceGroupARN :: !(Maybe Text)
    , _uaApplicationName  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateApplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaApplicationARN'
--
-- * 'uaResourceGroupARN'
--
-- * 'uaApplicationName'
updateApplication
    :: UpdateApplication
updateApplication =
    UpdateApplication'
    { _uaApplicationARN = Nothing
    , _uaResourceGroupARN = Nothing
    , _uaApplicationName = Nothing
    }

-- | Application ARN that you want to update.
uaApplicationARN :: Lens' UpdateApplication (Maybe Text)
uaApplicationARN = lens _uaApplicationARN (\ s a -> s{_uaApplicationARN = a});

-- | The resource group ARN that you want to update.
uaResourceGroupARN :: Lens' UpdateApplication (Maybe Text)
uaResourceGroupARN = lens _uaResourceGroupARN (\ s a -> s{_uaResourceGroupARN = a});

-- | Application name that you want to update.
uaApplicationName :: Lens' UpdateApplication (Maybe Text)
uaApplicationName = lens _uaApplicationName (\ s a -> s{_uaApplicationName = a});

instance AWSRequest UpdateApplication where
        type Rs UpdateApplication = UpdateApplicationResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 UpdateApplicationResponse' <$>
                   (x .?> "message") <*> (pure (fromEnum s)))

instance ToHeaders UpdateApplication where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.UpdateApplication" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateApplication where
        toJSON UpdateApplication'{..}
          = object
              (catMaybes
                 [("applicationArn" .=) <$> _uaApplicationARN,
                  ("resourceGroupArn" .=) <$> _uaResourceGroupARN,
                  ("applicationName" .=) <$> _uaApplicationName])

instance ToPath UpdateApplication where
        toPath = const "/"

instance ToQuery UpdateApplication where
        toQuery = const mempty

-- | /See:/ 'updateApplicationResponse' smart constructor.
data UpdateApplicationResponse = UpdateApplicationResponse'
    { _uarsMessage        :: !(Maybe Text)
    , _uarsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateApplicationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uarsMessage'
--
-- * 'uarsResponseStatus'
updateApplicationResponse
    :: Int -- ^ 'uarsResponseStatus'
    -> UpdateApplicationResponse
updateApplicationResponse pResponseStatus_ =
    UpdateApplicationResponse'
    { _uarsMessage = Nothing
    , _uarsResponseStatus = pResponseStatus_
    }

-- | Confirmation details of the action performed.
uarsMessage :: Lens' UpdateApplicationResponse (Maybe Text)
uarsMessage = lens _uarsMessage (\ s a -> s{_uarsMessage = a});

-- | The response status code.
uarsResponseStatus :: Lens' UpdateApplicationResponse Int
uarsResponseStatus = lens _uarsResponseStatus (\ s a -> s{_uarsResponseStatus = a});
