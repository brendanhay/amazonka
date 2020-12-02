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
-- Module      : Network.AWS.Glue.DeleteTrigger
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified trigger. If the trigger is not found, no exception is thrown.
--
--
module Network.AWS.Glue.DeleteTrigger
    (
    -- * Creating a Request
      deleteTrigger
    , DeleteTrigger
    -- * Request Lenses
    , dttName

    -- * Destructuring the Response
    , deleteTriggerResponse
    , DeleteTriggerResponse
    -- * Response Lenses
    , delrsName
    , delrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteTrigger' smart constructor.
newtype DeleteTrigger = DeleteTrigger'
  { _dttName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTrigger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dttName' - The name of the trigger to delete.
deleteTrigger
    :: Text -- ^ 'dttName'
    -> DeleteTrigger
deleteTrigger pName_ = DeleteTrigger' {_dttName = pName_}


-- | The name of the trigger to delete.
dttName :: Lens' DeleteTrigger Text
dttName = lens _dttName (\ s a -> s{_dttName = a})

instance AWSRequest DeleteTrigger where
        type Rs DeleteTrigger = DeleteTriggerResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 DeleteTriggerResponse' <$>
                   (x .?> "Name") <*> (pure (fromEnum s)))

instance Hashable DeleteTrigger where

instance NFData DeleteTrigger where

instance ToHeaders DeleteTrigger where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.DeleteTrigger" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteTrigger where
        toJSON DeleteTrigger'{..}
          = object (catMaybes [Just ("Name" .= _dttName)])

instance ToPath DeleteTrigger where
        toPath = const "/"

instance ToQuery DeleteTrigger where
        toQuery = const mempty

-- | /See:/ 'deleteTriggerResponse' smart constructor.
data DeleteTriggerResponse = DeleteTriggerResponse'
  { _delrsName           :: !(Maybe Text)
  , _delrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTriggerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsName' - The name of the trigger that was deleted.
--
-- * 'delrsResponseStatus' - -- | The response status code.
deleteTriggerResponse
    :: Int -- ^ 'delrsResponseStatus'
    -> DeleteTriggerResponse
deleteTriggerResponse pResponseStatus_ =
  DeleteTriggerResponse'
    {_delrsName = Nothing, _delrsResponseStatus = pResponseStatus_}


-- | The name of the trigger that was deleted.
delrsName :: Lens' DeleteTriggerResponse (Maybe Text)
delrsName = lens _delrsName (\ s a -> s{_delrsName = a})

-- | -- | The response status code.
delrsResponseStatus :: Lens' DeleteTriggerResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\ s a -> s{_delrsResponseStatus = a})

instance NFData DeleteTriggerResponse where
