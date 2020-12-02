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
-- Module      : Network.AWS.IoT.CreateThingGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a thing group.
--
--
module Network.AWS.IoT.CreateThingGroup
    (
    -- * Creating a Request
      createThingGroup
    , CreateThingGroup
    -- * Request Lenses
    , ctgParentGroupName
    , ctgThingGroupProperties
    , ctgThingGroupName

    -- * Destructuring the Response
    , createThingGroupResponse
    , CreateThingGroupResponse
    -- * Response Lenses
    , ctgrsThingGroupARN
    , ctgrsThingGroupId
    , ctgrsThingGroupName
    , ctgrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createThingGroup' smart constructor.
data CreateThingGroup = CreateThingGroup'
  { _ctgParentGroupName      :: !(Maybe Text)
  , _ctgThingGroupProperties :: !(Maybe ThingGroupProperties)
  , _ctgThingGroupName       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateThingGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctgParentGroupName' - The name of the parent thing group.
--
-- * 'ctgThingGroupProperties' - The thing group properties.
--
-- * 'ctgThingGroupName' - The thing group name to create.
createThingGroup
    :: Text -- ^ 'ctgThingGroupName'
    -> CreateThingGroup
createThingGroup pThingGroupName_ =
  CreateThingGroup'
    { _ctgParentGroupName = Nothing
    , _ctgThingGroupProperties = Nothing
    , _ctgThingGroupName = pThingGroupName_
    }


-- | The name of the parent thing group.
ctgParentGroupName :: Lens' CreateThingGroup (Maybe Text)
ctgParentGroupName = lens _ctgParentGroupName (\ s a -> s{_ctgParentGroupName = a})

-- | The thing group properties.
ctgThingGroupProperties :: Lens' CreateThingGroup (Maybe ThingGroupProperties)
ctgThingGroupProperties = lens _ctgThingGroupProperties (\ s a -> s{_ctgThingGroupProperties = a})

-- | The thing group name to create.
ctgThingGroupName :: Lens' CreateThingGroup Text
ctgThingGroupName = lens _ctgThingGroupName (\ s a -> s{_ctgThingGroupName = a})

instance AWSRequest CreateThingGroup where
        type Rs CreateThingGroup = CreateThingGroupResponse
        request = postJSON ioT
        response
          = receiveJSON
              (\ s h x ->
                 CreateThingGroupResponse' <$>
                   (x .?> "thingGroupArn") <*> (x .?> "thingGroupId")
                     <*> (x .?> "thingGroupName")
                     <*> (pure (fromEnum s)))

instance Hashable CreateThingGroup where

instance NFData CreateThingGroup where

instance ToHeaders CreateThingGroup where
        toHeaders = const mempty

instance ToJSON CreateThingGroup where
        toJSON CreateThingGroup'{..}
          = object
              (catMaybes
                 [("parentGroupName" .=) <$> _ctgParentGroupName,
                  ("thingGroupProperties" .=) <$>
                    _ctgThingGroupProperties])

instance ToPath CreateThingGroup where
        toPath CreateThingGroup'{..}
          = mconcat ["/thing-groups/", toBS _ctgThingGroupName]

instance ToQuery CreateThingGroup where
        toQuery = const mempty

-- | /See:/ 'createThingGroupResponse' smart constructor.
data CreateThingGroupResponse = CreateThingGroupResponse'
  { _ctgrsThingGroupARN  :: !(Maybe Text)
  , _ctgrsThingGroupId   :: !(Maybe Text)
  , _ctgrsThingGroupName :: !(Maybe Text)
  , _ctgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateThingGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctgrsThingGroupARN' - The thing group ARN.
--
-- * 'ctgrsThingGroupId' - The thing group ID.
--
-- * 'ctgrsThingGroupName' - The thing group name.
--
-- * 'ctgrsResponseStatus' - -- | The response status code.
createThingGroupResponse
    :: Int -- ^ 'ctgrsResponseStatus'
    -> CreateThingGroupResponse
createThingGroupResponse pResponseStatus_ =
  CreateThingGroupResponse'
    { _ctgrsThingGroupARN = Nothing
    , _ctgrsThingGroupId = Nothing
    , _ctgrsThingGroupName = Nothing
    , _ctgrsResponseStatus = pResponseStatus_
    }


-- | The thing group ARN.
ctgrsThingGroupARN :: Lens' CreateThingGroupResponse (Maybe Text)
ctgrsThingGroupARN = lens _ctgrsThingGroupARN (\ s a -> s{_ctgrsThingGroupARN = a})

-- | The thing group ID.
ctgrsThingGroupId :: Lens' CreateThingGroupResponse (Maybe Text)
ctgrsThingGroupId = lens _ctgrsThingGroupId (\ s a -> s{_ctgrsThingGroupId = a})

-- | The thing group name.
ctgrsThingGroupName :: Lens' CreateThingGroupResponse (Maybe Text)
ctgrsThingGroupName = lens _ctgrsThingGroupName (\ s a -> s{_ctgrsThingGroupName = a})

-- | -- | The response status code.
ctgrsResponseStatus :: Lens' CreateThingGroupResponse Int
ctgrsResponseStatus = lens _ctgrsResponseStatus (\ s a -> s{_ctgrsResponseStatus = a})

instance NFData CreateThingGroupResponse where
