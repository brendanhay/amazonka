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
-- Module      : Network.AWS.IoT.UpdateThingGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a thing group.
--
--
module Network.AWS.IoT.UpdateThingGroup
    (
    -- * Creating a Request
      updateThingGroup
    , UpdateThingGroup
    -- * Request Lenses
    , utgExpectedVersion
    , utgThingGroupName
    , utgThingGroupProperties

    -- * Destructuring the Response
    , updateThingGroupResponse
    , UpdateThingGroupResponse
    -- * Response Lenses
    , utgrsVersion
    , utgrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateThingGroup' smart constructor.
data UpdateThingGroup = UpdateThingGroup'
  { _utgExpectedVersion      :: !(Maybe Integer)
  , _utgThingGroupName       :: !Text
  , _utgThingGroupProperties :: !ThingGroupProperties
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateThingGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utgExpectedVersion' - The expected version of the thing group. If this does not match the version of the thing group being updated, the update will fail.
--
-- * 'utgThingGroupName' - The thing group to update.
--
-- * 'utgThingGroupProperties' - The thing group properties.
updateThingGroup
    :: Text -- ^ 'utgThingGroupName'
    -> ThingGroupProperties -- ^ 'utgThingGroupProperties'
    -> UpdateThingGroup
updateThingGroup pThingGroupName_ pThingGroupProperties_ =
  UpdateThingGroup'
    { _utgExpectedVersion = Nothing
    , _utgThingGroupName = pThingGroupName_
    , _utgThingGroupProperties = pThingGroupProperties_
    }


-- | The expected version of the thing group. If this does not match the version of the thing group being updated, the update will fail.
utgExpectedVersion :: Lens' UpdateThingGroup (Maybe Integer)
utgExpectedVersion = lens _utgExpectedVersion (\ s a -> s{_utgExpectedVersion = a})

-- | The thing group to update.
utgThingGroupName :: Lens' UpdateThingGroup Text
utgThingGroupName = lens _utgThingGroupName (\ s a -> s{_utgThingGroupName = a})

-- | The thing group properties.
utgThingGroupProperties :: Lens' UpdateThingGroup ThingGroupProperties
utgThingGroupProperties = lens _utgThingGroupProperties (\ s a -> s{_utgThingGroupProperties = a})

instance AWSRequest UpdateThingGroup where
        type Rs UpdateThingGroup = UpdateThingGroupResponse
        request = patchJSON ioT
        response
          = receiveJSON
              (\ s h x ->
                 UpdateThingGroupResponse' <$>
                   (x .?> "version") <*> (pure (fromEnum s)))

instance Hashable UpdateThingGroup where

instance NFData UpdateThingGroup where

instance ToHeaders UpdateThingGroup where
        toHeaders = const mempty

instance ToJSON UpdateThingGroup where
        toJSON UpdateThingGroup'{..}
          = object
              (catMaybes
                 [("expectedVersion" .=) <$> _utgExpectedVersion,
                  Just
                    ("thingGroupProperties" .=
                       _utgThingGroupProperties)])

instance ToPath UpdateThingGroup where
        toPath UpdateThingGroup'{..}
          = mconcat ["/thing-groups/", toBS _utgThingGroupName]

instance ToQuery UpdateThingGroup where
        toQuery = const mempty

-- | /See:/ 'updateThingGroupResponse' smart constructor.
data UpdateThingGroupResponse = UpdateThingGroupResponse'
  { _utgrsVersion        :: !(Maybe Integer)
  , _utgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateThingGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utgrsVersion' - The version of the updated thing group.
--
-- * 'utgrsResponseStatus' - -- | The response status code.
updateThingGroupResponse
    :: Int -- ^ 'utgrsResponseStatus'
    -> UpdateThingGroupResponse
updateThingGroupResponse pResponseStatus_ =
  UpdateThingGroupResponse'
    {_utgrsVersion = Nothing, _utgrsResponseStatus = pResponseStatus_}


-- | The version of the updated thing group.
utgrsVersion :: Lens' UpdateThingGroupResponse (Maybe Integer)
utgrsVersion = lens _utgrsVersion (\ s a -> s{_utgrsVersion = a})

-- | -- | The response status code.
utgrsResponseStatus :: Lens' UpdateThingGroupResponse Int
utgrsResponseStatus = lens _utgrsResponseStatus (\ s a -> s{_utgrsResponseStatus = a})

instance NFData UpdateThingGroupResponse where
