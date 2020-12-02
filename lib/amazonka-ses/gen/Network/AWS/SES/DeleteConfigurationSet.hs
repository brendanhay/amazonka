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
-- Module      : Network.AWS.SES.DeleteConfigurationSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
--
-- You can execute this operation no more than once per second.
--
module Network.AWS.SES.DeleteConfigurationSet
    (
    -- * Creating a Request
      deleteConfigurationSet
    , DeleteConfigurationSet
    -- * Request Lenses
    , dConfigurationSetName

    -- * Destructuring the Response
    , deleteConfigurationSetResponse
    , DeleteConfigurationSetResponse
    -- * Response Lenses
    , drsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to delete a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
--
--
-- /See:/ 'deleteConfigurationSet' smart constructor.
newtype DeleteConfigurationSet = DeleteConfigurationSet'
  { _dConfigurationSetName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteConfigurationSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dConfigurationSetName' - The name of the configuration set to delete.
deleteConfigurationSet
    :: Text -- ^ 'dConfigurationSetName'
    -> DeleteConfigurationSet
deleteConfigurationSet pConfigurationSetName_ =
  DeleteConfigurationSet' {_dConfigurationSetName = pConfigurationSetName_}


-- | The name of the configuration set to delete.
dConfigurationSetName :: Lens' DeleteConfigurationSet Text
dConfigurationSetName = lens _dConfigurationSetName (\ s a -> s{_dConfigurationSetName = a})

instance AWSRequest DeleteConfigurationSet where
        type Rs DeleteConfigurationSet =
             DeleteConfigurationSetResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "DeleteConfigurationSetResult"
              (\ s h x ->
                 DeleteConfigurationSetResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteConfigurationSet where

instance NFData DeleteConfigurationSet where

instance ToHeaders DeleteConfigurationSet where
        toHeaders = const mempty

instance ToPath DeleteConfigurationSet where
        toPath = const "/"

instance ToQuery DeleteConfigurationSet where
        toQuery DeleteConfigurationSet'{..}
          = mconcat
              ["Action" =:
                 ("DeleteConfigurationSet" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "ConfigurationSetName" =: _dConfigurationSetName]

-- | An empty element returned on a successful request.
--
--
--
-- /See:/ 'deleteConfigurationSetResponse' smart constructor.
newtype DeleteConfigurationSetResponse = DeleteConfigurationSetResponse'
  { _drsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteConfigurationSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsResponseStatus' - -- | The response status code.
deleteConfigurationSetResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeleteConfigurationSetResponse
deleteConfigurationSetResponse pResponseStatus_ =
  DeleteConfigurationSetResponse' {_drsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteConfigurationSetResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DeleteConfigurationSetResponse where
