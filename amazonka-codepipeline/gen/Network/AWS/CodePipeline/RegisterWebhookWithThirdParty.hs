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
-- Module      : Network.AWS.CodePipeline.RegisterWebhookWithThirdParty
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures a connection between the webhook that was created and the external tool with events to be detected.
--
--
module Network.AWS.CodePipeline.RegisterWebhookWithThirdParty
    (
    -- * Creating a Request
      registerWebhookWithThirdParty
    , RegisterWebhookWithThirdParty
    -- * Request Lenses
    , rwwtpWebhookName

    -- * Destructuring the Response
    , registerWebhookWithThirdPartyResponse
    , RegisterWebhookWithThirdPartyResponse
    -- * Response Lenses
    , rwwtprsResponseStatus
    ) where

import Network.AWS.CodePipeline.Types
import Network.AWS.CodePipeline.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'registerWebhookWithThirdParty' smart constructor.
newtype RegisterWebhookWithThirdParty = RegisterWebhookWithThirdParty'
  { _rwwtpWebhookName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterWebhookWithThirdParty' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rwwtpWebhookName' - The name of an existing webhook created with PutWebhook to register with a supported third party.
registerWebhookWithThirdParty
    :: RegisterWebhookWithThirdParty
registerWebhookWithThirdParty =
  RegisterWebhookWithThirdParty' {_rwwtpWebhookName = Nothing}


-- | The name of an existing webhook created with PutWebhook to register with a supported third party.
rwwtpWebhookName :: Lens' RegisterWebhookWithThirdParty (Maybe Text)
rwwtpWebhookName = lens _rwwtpWebhookName (\ s a -> s{_rwwtpWebhookName = a})

instance AWSRequest RegisterWebhookWithThirdParty
         where
        type Rs RegisterWebhookWithThirdParty =
             RegisterWebhookWithThirdPartyResponse
        request = postJSON codePipeline
        response
          = receiveEmpty
              (\ s h x ->
                 RegisterWebhookWithThirdPartyResponse' <$>
                   (pure (fromEnum s)))

instance Hashable RegisterWebhookWithThirdParty where

instance NFData RegisterWebhookWithThirdParty where

instance ToHeaders RegisterWebhookWithThirdParty
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodePipeline_20150709.RegisterWebhookWithThirdParty"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RegisterWebhookWithThirdParty where
        toJSON RegisterWebhookWithThirdParty'{..}
          = object
              (catMaybes
                 [("webhookName" .=) <$> _rwwtpWebhookName])

instance ToPath RegisterWebhookWithThirdParty where
        toPath = const "/"

instance ToQuery RegisterWebhookWithThirdParty where
        toQuery = const mempty

-- | /See:/ 'registerWebhookWithThirdPartyResponse' smart constructor.
newtype RegisterWebhookWithThirdPartyResponse = RegisterWebhookWithThirdPartyResponse'
  { _rwwtprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterWebhookWithThirdPartyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rwwtprsResponseStatus' - -- | The response status code.
registerWebhookWithThirdPartyResponse
    :: Int -- ^ 'rwwtprsResponseStatus'
    -> RegisterWebhookWithThirdPartyResponse
registerWebhookWithThirdPartyResponse pResponseStatus_ =
  RegisterWebhookWithThirdPartyResponse'
    {_rwwtprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
rwwtprsResponseStatus :: Lens' RegisterWebhookWithThirdPartyResponse Int
rwwtprsResponseStatus = lens _rwwtprsResponseStatus (\ s a -> s{_rwwtprsResponseStatus = a})

instance NFData RegisterWebhookWithThirdPartyResponse
         where
