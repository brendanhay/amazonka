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
-- Module      : Network.AWS.IoT.RemoveThingFromThingGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove the specified thing from the specified group.
--
--
module Network.AWS.IoT.RemoveThingFromThingGroup
    (
    -- * Creating a Request
      removeThingFromThingGroup
    , RemoveThingFromThingGroup
    -- * Request Lenses
    , rtftgThingGroupARN
    , rtftgThingARN
    , rtftgThingGroupName
    , rtftgThingName

    -- * Destructuring the Response
    , removeThingFromThingGroupResponse
    , RemoveThingFromThingGroupResponse
    -- * Response Lenses
    , rtftgrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'removeThingFromThingGroup' smart constructor.
data RemoveThingFromThingGroup = RemoveThingFromThingGroup'
  { _rtftgThingGroupARN  :: !(Maybe Text)
  , _rtftgThingARN       :: !(Maybe Text)
  , _rtftgThingGroupName :: !(Maybe Text)
  , _rtftgThingName      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveThingFromThingGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtftgThingGroupARN' - The group ARN.
--
-- * 'rtftgThingARN' - The ARN of the thing to remove from the group.
--
-- * 'rtftgThingGroupName' - The group name.
--
-- * 'rtftgThingName' - The name of the thing to remove from the group.
removeThingFromThingGroup
    :: RemoveThingFromThingGroup
removeThingFromThingGroup =
  RemoveThingFromThingGroup'
    { _rtftgThingGroupARN = Nothing
    , _rtftgThingARN = Nothing
    , _rtftgThingGroupName = Nothing
    , _rtftgThingName = Nothing
    }


-- | The group ARN.
rtftgThingGroupARN :: Lens' RemoveThingFromThingGroup (Maybe Text)
rtftgThingGroupARN = lens _rtftgThingGroupARN (\ s a -> s{_rtftgThingGroupARN = a})

-- | The ARN of the thing to remove from the group.
rtftgThingARN :: Lens' RemoveThingFromThingGroup (Maybe Text)
rtftgThingARN = lens _rtftgThingARN (\ s a -> s{_rtftgThingARN = a})

-- | The group name.
rtftgThingGroupName :: Lens' RemoveThingFromThingGroup (Maybe Text)
rtftgThingGroupName = lens _rtftgThingGroupName (\ s a -> s{_rtftgThingGroupName = a})

-- | The name of the thing to remove from the group.
rtftgThingName :: Lens' RemoveThingFromThingGroup (Maybe Text)
rtftgThingName = lens _rtftgThingName (\ s a -> s{_rtftgThingName = a})

instance AWSRequest RemoveThingFromThingGroup where
        type Rs RemoveThingFromThingGroup =
             RemoveThingFromThingGroupResponse
        request = putJSON ioT
        response
          = receiveEmpty
              (\ s h x ->
                 RemoveThingFromThingGroupResponse' <$>
                   (pure (fromEnum s)))

instance Hashable RemoveThingFromThingGroup where

instance NFData RemoveThingFromThingGroup where

instance ToHeaders RemoveThingFromThingGroup where
        toHeaders = const mempty

instance ToJSON RemoveThingFromThingGroup where
        toJSON RemoveThingFromThingGroup'{..}
          = object
              (catMaybes
                 [("thingGroupArn" .=) <$> _rtftgThingGroupARN,
                  ("thingArn" .=) <$> _rtftgThingARN,
                  ("thingGroupName" .=) <$> _rtftgThingGroupName,
                  ("thingName" .=) <$> _rtftgThingName])

instance ToPath RemoveThingFromThingGroup where
        toPath
          = const "/thing-groups/removeThingFromThingGroup"

instance ToQuery RemoveThingFromThingGroup where
        toQuery = const mempty

-- | /See:/ 'removeThingFromThingGroupResponse' smart constructor.
newtype RemoveThingFromThingGroupResponse = RemoveThingFromThingGroupResponse'
  { _rtftgrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveThingFromThingGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtftgrsResponseStatus' - -- | The response status code.
removeThingFromThingGroupResponse
    :: Int -- ^ 'rtftgrsResponseStatus'
    -> RemoveThingFromThingGroupResponse
removeThingFromThingGroupResponse pResponseStatus_ =
  RemoveThingFromThingGroupResponse' {_rtftgrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
rtftgrsResponseStatus :: Lens' RemoveThingFromThingGroupResponse Int
rtftgrsResponseStatus = lens _rtftgrsResponseStatus (\ s a -> s{_rtftgrsResponseStatus = a})

instance NFData RemoveThingFromThingGroupResponse
         where
