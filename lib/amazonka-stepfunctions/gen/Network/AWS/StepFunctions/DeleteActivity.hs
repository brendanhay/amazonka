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
-- Module      : Network.AWS.StepFunctions.DeleteActivity
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an activity.
--
--
module Network.AWS.StepFunctions.DeleteActivity
    (
    -- * Creating a Request
      deleteActivity
    , DeleteActivity
    -- * Request Lenses
    , daActivityARN

    -- * Destructuring the Response
    , deleteActivityResponse
    , DeleteActivityResponse
    -- * Response Lenses
    , darsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StepFunctions.Types
import Network.AWS.StepFunctions.Types.Product

-- | /See:/ 'deleteActivity' smart constructor.
newtype DeleteActivity = DeleteActivity'
  { _daActivityARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daActivityARN' - The Amazon Resource Name (ARN) of the activity to delete.
deleteActivity
    :: Text -- ^ 'daActivityARN'
    -> DeleteActivity
deleteActivity pActivityARN_ = DeleteActivity' {_daActivityARN = pActivityARN_}


-- | The Amazon Resource Name (ARN) of the activity to delete.
daActivityARN :: Lens' DeleteActivity Text
daActivityARN = lens _daActivityARN (\ s a -> s{_daActivityARN = a})

instance AWSRequest DeleteActivity where
        type Rs DeleteActivity = DeleteActivityResponse
        request = postJSON stepFunctions
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteActivityResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteActivity where

instance NFData DeleteActivity where

instance ToHeaders DeleteActivity where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSStepFunctions.DeleteActivity" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON DeleteActivity where
        toJSON DeleteActivity'{..}
          = object
              (catMaybes [Just ("activityArn" .= _daActivityARN)])

instance ToPath DeleteActivity where
        toPath = const "/"

instance ToQuery DeleteActivity where
        toQuery = const mempty

-- | /See:/ 'deleteActivityResponse' smart constructor.
newtype DeleteActivityResponse = DeleteActivityResponse'
  { _darsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteActivityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darsResponseStatus' - -- | The response status code.
deleteActivityResponse
    :: Int -- ^ 'darsResponseStatus'
    -> DeleteActivityResponse
deleteActivityResponse pResponseStatus_ =
  DeleteActivityResponse' {_darsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
darsResponseStatus :: Lens' DeleteActivityResponse Int
darsResponseStatus = lens _darsResponseStatus (\ s a -> s{_darsResponseStatus = a})

instance NFData DeleteActivityResponse where
