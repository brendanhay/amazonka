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
-- Module      : Network.AWS.IoT.DeleteThingType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified thing type . You cannot delete a thing type if it has things associated with it. To delete a thing type, first mark it as deprecated by calling 'DeprecateThingType' , then remove any associated things by calling 'UpdateThing' to change the thing type on any associated thing, and finally use 'DeleteThingType' to delete the thing type.
--
--
module Network.AWS.IoT.DeleteThingType
    (
    -- * Creating a Request
      deleteThingType
    , DeleteThingType
    -- * Request Lenses
    , dttThingTypeName

    -- * Destructuring the Response
    , deleteThingTypeResponse
    , DeleteThingTypeResponse
    -- * Response Lenses
    , dttrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the DeleteThingType operation.
--
--
--
-- /See:/ 'deleteThingType' smart constructor.
newtype DeleteThingType = DeleteThingType'
  { _dttThingTypeName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteThingType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dttThingTypeName' - The name of the thing type.
deleteThingType
    :: Text -- ^ 'dttThingTypeName'
    -> DeleteThingType
deleteThingType pThingTypeName_ =
  DeleteThingType' {_dttThingTypeName = pThingTypeName_}


-- | The name of the thing type.
dttThingTypeName :: Lens' DeleteThingType Text
dttThingTypeName = lens _dttThingTypeName (\ s a -> s{_dttThingTypeName = a})

instance AWSRequest DeleteThingType where
        type Rs DeleteThingType = DeleteThingTypeResponse
        request = delete ioT
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteThingTypeResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteThingType where

instance NFData DeleteThingType where

instance ToHeaders DeleteThingType where
        toHeaders = const mempty

instance ToPath DeleteThingType where
        toPath DeleteThingType'{..}
          = mconcat ["/thing-types/", toBS _dttThingTypeName]

instance ToQuery DeleteThingType where
        toQuery = const mempty

-- | The output for the DeleteThingType operation.
--
--
--
-- /See:/ 'deleteThingTypeResponse' smart constructor.
newtype DeleteThingTypeResponse = DeleteThingTypeResponse'
  { _dttrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteThingTypeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dttrsResponseStatus' - -- | The response status code.
deleteThingTypeResponse
    :: Int -- ^ 'dttrsResponseStatus'
    -> DeleteThingTypeResponse
deleteThingTypeResponse pResponseStatus_ =
  DeleteThingTypeResponse' {_dttrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dttrsResponseStatus :: Lens' DeleteThingTypeResponse Int
dttrsResponseStatus = lens _dttrsResponseStatus (\ s a -> s{_dttrsResponseStatus = a})

instance NFData DeleteThingTypeResponse where
