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
-- Module      : Network.AWS.Inspector.RegisterCrossAccountAccessRole
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Register the role that Inspector uses to list your EC2 instances during
-- the assessment.
module Network.AWS.Inspector.RegisterCrossAccountAccessRole
    (
    -- * Creating a Request
      registerCrossAccountAccessRole
    , RegisterCrossAccountAccessRole
    -- * Request Lenses
    , rcaarRoleARN

    -- * Destructuring the Response
    , registerCrossAccountAccessRoleResponse
    , RegisterCrossAccountAccessRoleResponse
    -- * Response Lenses
    , rcaarrsMessage
    , rcaarrsResponseStatus
    ) where

import           Network.AWS.Inspector.Types
import           Network.AWS.Inspector.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'registerCrossAccountAccessRole' smart constructor.
newtype RegisterCrossAccountAccessRole = RegisterCrossAccountAccessRole'
    { _rcaarRoleARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RegisterCrossAccountAccessRole' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcaarRoleARN'
registerCrossAccountAccessRole
    :: Text -- ^ 'rcaarRoleARN'
    -> RegisterCrossAccountAccessRole
registerCrossAccountAccessRole pRoleARN_ =
    RegisterCrossAccountAccessRole'
    { _rcaarRoleARN = pRoleARN_
    }

-- | The ARN of the IAM role that Inspector uses to list your EC2 instances
-- during the assessment.
rcaarRoleARN :: Lens' RegisterCrossAccountAccessRole Text
rcaarRoleARN = lens _rcaarRoleARN (\ s a -> s{_rcaarRoleARN = a});

instance AWSRequest RegisterCrossAccountAccessRole
         where
        type Rs RegisterCrossAccountAccessRole =
             RegisterCrossAccountAccessRoleResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 RegisterCrossAccountAccessRoleResponse' <$>
                   (x .?> "message") <*> (pure (fromEnum s)))

instance Hashable RegisterCrossAccountAccessRole

instance NFData RegisterCrossAccountAccessRole

instance ToHeaders RegisterCrossAccountAccessRole
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.RegisterCrossAccountAccessRole" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RegisterCrossAccountAccessRole where
        toJSON RegisterCrossAccountAccessRole'{..}
          = object
              (catMaybes [Just ("roleArn" .= _rcaarRoleARN)])

instance ToPath RegisterCrossAccountAccessRole where
        toPath = const "/"

instance ToQuery RegisterCrossAccountAccessRole where
        toQuery = const mempty

-- | /See:/ 'registerCrossAccountAccessRoleResponse' smart constructor.
data RegisterCrossAccountAccessRoleResponse = RegisterCrossAccountAccessRoleResponse'
    { _rcaarrsMessage        :: !(Maybe Text)
    , _rcaarrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RegisterCrossAccountAccessRoleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcaarrsMessage'
--
-- * 'rcaarrsResponseStatus'
registerCrossAccountAccessRoleResponse
    :: Int -- ^ 'rcaarrsResponseStatus'
    -> RegisterCrossAccountAccessRoleResponse
registerCrossAccountAccessRoleResponse pResponseStatus_ =
    RegisterCrossAccountAccessRoleResponse'
    { _rcaarrsMessage = Nothing
    , _rcaarrsResponseStatus = pResponseStatus_
    }

-- | Confirmation details of the action performed.
rcaarrsMessage :: Lens' RegisterCrossAccountAccessRoleResponse (Maybe Text)
rcaarrsMessage = lens _rcaarrsMessage (\ s a -> s{_rcaarrsMessage = a});

-- | The response status code.
rcaarrsResponseStatus :: Lens' RegisterCrossAccountAccessRoleResponse Int
rcaarrsResponseStatus = lens _rcaarrsResponseStatus (\ s a -> s{_rcaarrsResponseStatus = a});
