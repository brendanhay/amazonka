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
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified thing.
--
--
module Network.AWS.IoT.DeleteThing
    (
    -- * Creating a Request
      deleteThing
    , DeleteThing
    -- * Request Lenses
    , dtExpectedVersion
    , dtThingName

    -- * Destructuring the Response
    , deleteThingResponse
    , DeleteThingResponse
    -- * Response Lenses
    , delrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the DeleteThing operation.
--
--
--
-- /See:/ 'deleteThing' smart constructor.
data DeleteThing = DeleteThing'
  { _dtExpectedVersion :: !(Maybe Integer)
  , _dtThingName       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteThing' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtExpectedVersion' - The expected version of the thing record in the registry. If the version of the record in the registry does not match the expected version specified in the request, the @DeleteThing@ request is rejected with a @VersionConflictException@ .
--
-- * 'dtThingName' - The name of the thing to delete.
deleteThing
    :: Text -- ^ 'dtThingName'
    -> DeleteThing
deleteThing pThingName_ =
  DeleteThing' {_dtExpectedVersion = Nothing, _dtThingName = pThingName_}


-- | The expected version of the thing record in the registry. If the version of the record in the registry does not match the expected version specified in the request, the @DeleteThing@ request is rejected with a @VersionConflictException@ .
dtExpectedVersion :: Lens' DeleteThing (Maybe Integer)
dtExpectedVersion = lens _dtExpectedVersion (\ s a -> s{_dtExpectedVersion = a});

-- | The name of the thing to delete.
dtThingName :: Lens' DeleteThing Text
dtThingName = lens _dtThingName (\ s a -> s{_dtThingName = a});

instance AWSRequest DeleteThing where
        type Rs DeleteThing = DeleteThingResponse
        request = delete ioT
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteThingResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteThing where

instance NFData DeleteThing where

instance ToHeaders DeleteThing where
        toHeaders = const mempty

instance ToPath DeleteThing where
        toPath DeleteThing'{..}
          = mconcat ["/things/", toBS _dtThingName]

instance ToQuery DeleteThing where
        toQuery DeleteThing'{..}
          = mconcat ["expectedVersion" =: _dtExpectedVersion]

-- | The output of the DeleteThing operation.
--
--
--
-- /See:/ 'deleteThingResponse' smart constructor.
newtype DeleteThingResponse = DeleteThingResponse'
  { _delrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteThingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsResponseStatus' - -- | The response status code.
deleteThingResponse
    :: Int -- ^ 'delrsResponseStatus'
    -> DeleteThingResponse
deleteThingResponse pResponseStatus_ =
  DeleteThingResponse' {_delrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
delrsResponseStatus :: Lens' DeleteThingResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\ s a -> s{_delrsResponseStatus = a});

instance NFData DeleteThingResponse where
