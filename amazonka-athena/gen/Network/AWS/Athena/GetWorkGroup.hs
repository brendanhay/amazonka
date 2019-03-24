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
-- Module      : Network.AWS.Athena.GetWorkGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the workgroup with the specified name.
--
--
module Network.AWS.Athena.GetWorkGroup
    (
    -- * Creating a Request
      getWorkGroup
    , GetWorkGroup
    -- * Request Lenses
    , gwgWorkGroup

    -- * Destructuring the Response
    , getWorkGroupResponse
    , GetWorkGroupResponse
    -- * Response Lenses
    , gwgrsWorkGroup
    , gwgrsResponseStatus
    ) where

import Network.AWS.Athena.Types
import Network.AWS.Athena.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getWorkGroup' smart constructor.
newtype GetWorkGroup = GetWorkGroup'
  { _gwgWorkGroup :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetWorkGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gwgWorkGroup' - The name of the workgroup.
getWorkGroup
    :: Text -- ^ 'gwgWorkGroup'
    -> GetWorkGroup
getWorkGroup pWorkGroup_ = GetWorkGroup' {_gwgWorkGroup = pWorkGroup_}


-- | The name of the workgroup.
gwgWorkGroup :: Lens' GetWorkGroup Text
gwgWorkGroup = lens _gwgWorkGroup (\ s a -> s{_gwgWorkGroup = a})

instance AWSRequest GetWorkGroup where
        type Rs GetWorkGroup = GetWorkGroupResponse
        request = postJSON athena
        response
          = receiveJSON
              (\ s h x ->
                 GetWorkGroupResponse' <$>
                   (x .?> "WorkGroup") <*> (pure (fromEnum s)))

instance Hashable GetWorkGroup where

instance NFData GetWorkGroup where

instance ToHeaders GetWorkGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonAthena.GetWorkGroup" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetWorkGroup where
        toJSON GetWorkGroup'{..}
          = object
              (catMaybes [Just ("WorkGroup" .= _gwgWorkGroup)])

instance ToPath GetWorkGroup where
        toPath = const "/"

instance ToQuery GetWorkGroup where
        toQuery = const mempty

-- | /See:/ 'getWorkGroupResponse' smart constructor.
data GetWorkGroupResponse = GetWorkGroupResponse'
  { _gwgrsWorkGroup      :: !(Maybe WorkGroup)
  , _gwgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetWorkGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gwgrsWorkGroup' - Information about the workgroup.
--
-- * 'gwgrsResponseStatus' - -- | The response status code.
getWorkGroupResponse
    :: Int -- ^ 'gwgrsResponseStatus'
    -> GetWorkGroupResponse
getWorkGroupResponse pResponseStatus_ =
  GetWorkGroupResponse'
    {_gwgrsWorkGroup = Nothing, _gwgrsResponseStatus = pResponseStatus_}


-- | Information about the workgroup.
gwgrsWorkGroup :: Lens' GetWorkGroupResponse (Maybe WorkGroup)
gwgrsWorkGroup = lens _gwgrsWorkGroup (\ s a -> s{_gwgrsWorkGroup = a})

-- | -- | The response status code.
gwgrsResponseStatus :: Lens' GetWorkGroupResponse Int
gwgrsResponseStatus = lens _gwgrsResponseStatus (\ s a -> s{_gwgrsResponseStatus = a})

instance NFData GetWorkGroupResponse where
