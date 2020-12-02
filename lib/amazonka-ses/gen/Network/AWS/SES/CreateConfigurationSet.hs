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
-- Module      : Network.AWS.SES.CreateConfigurationSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a configuration set.
--
--
-- Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- You can execute this operation no more than once per second.
--
module Network.AWS.SES.CreateConfigurationSet
    (
    -- * Creating a Request
      createConfigurationSet
    , CreateConfigurationSet
    -- * Request Lenses
    , ccsConfigurationSet

    -- * Destructuring the Response
    , createConfigurationSetResponse
    , CreateConfigurationSetResponse
    -- * Response Lenses
    , ccsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to create a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
--
--
-- /See:/ 'createConfigurationSet' smart constructor.
newtype CreateConfigurationSet = CreateConfigurationSet'
  { _ccsConfigurationSet :: ConfigurationSet
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateConfigurationSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccsConfigurationSet' - A data structure that contains the name of the configuration set.
createConfigurationSet
    :: ConfigurationSet -- ^ 'ccsConfigurationSet'
    -> CreateConfigurationSet
createConfigurationSet pConfigurationSet_ =
  CreateConfigurationSet' {_ccsConfigurationSet = pConfigurationSet_}


-- | A data structure that contains the name of the configuration set.
ccsConfigurationSet :: Lens' CreateConfigurationSet ConfigurationSet
ccsConfigurationSet = lens _ccsConfigurationSet (\ s a -> s{_ccsConfigurationSet = a})

instance AWSRequest CreateConfigurationSet where
        type Rs CreateConfigurationSet =
             CreateConfigurationSetResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "CreateConfigurationSetResult"
              (\ s h x ->
                 CreateConfigurationSetResponse' <$>
                   (pure (fromEnum s)))

instance Hashable CreateConfigurationSet where

instance NFData CreateConfigurationSet where

instance ToHeaders CreateConfigurationSet where
        toHeaders = const mempty

instance ToPath CreateConfigurationSet where
        toPath = const "/"

instance ToQuery CreateConfigurationSet where
        toQuery CreateConfigurationSet'{..}
          = mconcat
              ["Action" =:
                 ("CreateConfigurationSet" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "ConfigurationSet" =: _ccsConfigurationSet]

-- | An empty element returned on a successful request.
--
--
--
-- /See:/ 'createConfigurationSetResponse' smart constructor.
newtype CreateConfigurationSetResponse = CreateConfigurationSetResponse'
  { _ccsrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateConfigurationSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccsrsResponseStatus' - -- | The response status code.
createConfigurationSetResponse
    :: Int -- ^ 'ccsrsResponseStatus'
    -> CreateConfigurationSetResponse
createConfigurationSetResponse pResponseStatus_ =
  CreateConfigurationSetResponse' {_ccsrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ccsrsResponseStatus :: Lens' CreateConfigurationSetResponse Int
ccsrsResponseStatus = lens _ccsrsResponseStatus (\ s a -> s{_ccsrsResponseStatus = a})

instance NFData CreateConfigurationSetResponse where
