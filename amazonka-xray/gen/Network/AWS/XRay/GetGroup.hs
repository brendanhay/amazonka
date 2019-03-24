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
-- Module      : Network.AWS.XRay.GetGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves group resource details.
--
--
module Network.AWS.XRay.GetGroup
    (
    -- * Creating a Request
      getGroup
    , GetGroup
    -- * Request Lenses
    , ggGroupARN
    , ggGroupName

    -- * Destructuring the Response
    , getGroupResponse
    , GetGroupResponse
    -- * Response Lenses
    , ggrsGroup
    , ggrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.XRay.Types
import Network.AWS.XRay.Types.Product

-- | /See:/ 'getGroup' smart constructor.
data GetGroup = GetGroup'
  { _ggGroupARN :: !(Maybe Text)
  , _ggGroupName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggGroupARN' - The ARN of the group that was generated on creation.
--
-- * 'ggGroupName' - The case-sensitive name of the group.
getGroup
    :: GetGroup
getGroup = GetGroup' {_ggGroupARN = Nothing, _ggGroupName = Nothing}


-- | The ARN of the group that was generated on creation.
ggGroupARN :: Lens' GetGroup (Maybe Text)
ggGroupARN = lens _ggGroupARN (\ s a -> s{_ggGroupARN = a})

-- | The case-sensitive name of the group.
ggGroupName :: Lens' GetGroup (Maybe Text)
ggGroupName = lens _ggGroupName (\ s a -> s{_ggGroupName = a})

instance AWSRequest GetGroup where
        type Rs GetGroup = GetGroupResponse
        request = postJSON xRay
        response
          = receiveJSON
              (\ s h x ->
                 GetGroupResponse' <$>
                   (x .?> "Group") <*> (pure (fromEnum s)))

instance Hashable GetGroup where

instance NFData GetGroup where

instance ToHeaders GetGroup where
        toHeaders = const mempty

instance ToJSON GetGroup where
        toJSON GetGroup'{..}
          = object
              (catMaybes
                 [("GroupARN" .=) <$> _ggGroupARN,
                  ("GroupName" .=) <$> _ggGroupName])

instance ToPath GetGroup where
        toPath = const "/GetGroup"

instance ToQuery GetGroup where
        toQuery = const mempty

-- | /See:/ 'getGroupResponse' smart constructor.
data GetGroupResponse = GetGroupResponse'
  { _ggrsGroup :: !(Maybe Group)
  , _ggrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggrsGroup' - The group that was requested. Contains the name of the group, the ARN of the group, and the filter expression that assigned to the group.
--
-- * 'ggrsResponseStatus' - -- | The response status code.
getGroupResponse
    :: Int -- ^ 'ggrsResponseStatus'
    -> GetGroupResponse
getGroupResponse pResponseStatus_ =
  GetGroupResponse'
    {_ggrsGroup = Nothing, _ggrsResponseStatus = pResponseStatus_}


-- | The group that was requested. Contains the name of the group, the ARN of the group, and the filter expression that assigned to the group.
ggrsGroup :: Lens' GetGroupResponse (Maybe Group)
ggrsGroup = lens _ggrsGroup (\ s a -> s{_ggrsGroup = a})

-- | -- | The response status code.
ggrsResponseStatus :: Lens' GetGroupResponse Int
ggrsResponseStatus = lens _ggrsResponseStatus (\ s a -> s{_ggrsResponseStatus = a})

instance NFData GetGroupResponse where
