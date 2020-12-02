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
-- Module      : Network.AWS.IoT.UpdateThingGroupsForThing
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the groups to which the thing belongs.
--
--
module Network.AWS.IoT.UpdateThingGroupsForThing
    (
    -- * Creating a Request
      updateThingGroupsForThing
    , UpdateThingGroupsForThing
    -- * Request Lenses
    , utgftThingGroupsToAdd
    , utgftThingGroupsToRemove
    , utgftThingName

    -- * Destructuring the Response
    , updateThingGroupsForThingResponse
    , UpdateThingGroupsForThingResponse
    -- * Response Lenses
    , utgftrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateThingGroupsForThing' smart constructor.
data UpdateThingGroupsForThing = UpdateThingGroupsForThing'
  { _utgftThingGroupsToAdd    :: !(Maybe [Text])
  , _utgftThingGroupsToRemove :: !(Maybe [Text])
  , _utgftThingName           :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateThingGroupsForThing' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utgftThingGroupsToAdd' - The groups to which the thing will be added.
--
-- * 'utgftThingGroupsToRemove' - The groups from which the thing will be removed.
--
-- * 'utgftThingName' - The thing whose group memberships will be updated.
updateThingGroupsForThing
    :: UpdateThingGroupsForThing
updateThingGroupsForThing =
  UpdateThingGroupsForThing'
    { _utgftThingGroupsToAdd = Nothing
    , _utgftThingGroupsToRemove = Nothing
    , _utgftThingName = Nothing
    }


-- | The groups to which the thing will be added.
utgftThingGroupsToAdd :: Lens' UpdateThingGroupsForThing [Text]
utgftThingGroupsToAdd = lens _utgftThingGroupsToAdd (\ s a -> s{_utgftThingGroupsToAdd = a}) . _Default . _Coerce

-- | The groups from which the thing will be removed.
utgftThingGroupsToRemove :: Lens' UpdateThingGroupsForThing [Text]
utgftThingGroupsToRemove = lens _utgftThingGroupsToRemove (\ s a -> s{_utgftThingGroupsToRemove = a}) . _Default . _Coerce

-- | The thing whose group memberships will be updated.
utgftThingName :: Lens' UpdateThingGroupsForThing (Maybe Text)
utgftThingName = lens _utgftThingName (\ s a -> s{_utgftThingName = a})

instance AWSRequest UpdateThingGroupsForThing where
        type Rs UpdateThingGroupsForThing =
             UpdateThingGroupsForThingResponse
        request = putJSON ioT
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateThingGroupsForThingResponse' <$>
                   (pure (fromEnum s)))

instance Hashable UpdateThingGroupsForThing where

instance NFData UpdateThingGroupsForThing where

instance ToHeaders UpdateThingGroupsForThing where
        toHeaders = const mempty

instance ToJSON UpdateThingGroupsForThing where
        toJSON UpdateThingGroupsForThing'{..}
          = object
              (catMaybes
                 [("thingGroupsToAdd" .=) <$> _utgftThingGroupsToAdd,
                  ("thingGroupsToRemove" .=) <$>
                    _utgftThingGroupsToRemove,
                  ("thingName" .=) <$> _utgftThingName])

instance ToPath UpdateThingGroupsForThing where
        toPath
          = const "/thing-groups/updateThingGroupsForThing"

instance ToQuery UpdateThingGroupsForThing where
        toQuery = const mempty

-- | /See:/ 'updateThingGroupsForThingResponse' smart constructor.
newtype UpdateThingGroupsForThingResponse = UpdateThingGroupsForThingResponse'
  { _utgftrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateThingGroupsForThingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utgftrsResponseStatus' - -- | The response status code.
updateThingGroupsForThingResponse
    :: Int -- ^ 'utgftrsResponseStatus'
    -> UpdateThingGroupsForThingResponse
updateThingGroupsForThingResponse pResponseStatus_ =
  UpdateThingGroupsForThingResponse' {_utgftrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
utgftrsResponseStatus :: Lens' UpdateThingGroupsForThingResponse Int
utgftrsResponseStatus = lens _utgftrsResponseStatus (\ s a -> s{_utgftrsResponseStatus = a})

instance NFData UpdateThingGroupsForThingResponse
         where
