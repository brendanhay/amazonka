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
-- Module      : Network.AWS.IoT.DeleteThing
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified thing from the Thing Registry.
module Network.AWS.IoT.DeleteThing
    (
    -- * Creating a Request
      deleteThing
    , DeleteThing
    -- * Request Lenses
    , dtThingName

    -- * Destructuring the Response
    , deleteThingResponse
    , DeleteThingResponse
    -- * Response Lenses
    , drsResponseStatus
    ) where

import           Network.AWS.IoT.Types
import           Network.AWS.IoT.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the DeleteThing operation.
--
-- /See:/ 'deleteThing' smart constructor.
newtype DeleteThing = DeleteThing'
    { _dtThingName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteThing' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtThingName'
deleteThing
    :: Text -- ^ 'dtThingName'
    -> DeleteThing
deleteThing pThingName_ =
    DeleteThing'
    { _dtThingName = pThingName_
    }

-- | The thing name.
dtThingName :: Lens' DeleteThing Text
dtThingName = lens _dtThingName (\ s a -> s{_dtThingName = a});

instance AWSRequest DeleteThing where
        type Rs DeleteThing = DeleteThingResponse
        request = delete ioT
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteThingResponse' <$> (pure (fromEnum s)))

instance ToHeaders DeleteThing where
        toHeaders = const mempty

instance ToPath DeleteThing where
        toPath DeleteThing'{..}
          = mconcat ["/things/", toBS _dtThingName]

instance ToQuery DeleteThing where
        toQuery = const mempty

-- | The output of the DeleteThing operation.
--
-- /See:/ 'deleteThingResponse' smart constructor.
newtype DeleteThingResponse = DeleteThingResponse'
    { _drsResponseStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteThingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsResponseStatus'
deleteThingResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeleteThingResponse
deleteThingResponse pResponseStatus_ =
    DeleteThingResponse'
    { _drsResponseStatus = pResponseStatus_
    }

-- | The response status code.
drsResponseStatus :: Lens' DeleteThingResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a});
