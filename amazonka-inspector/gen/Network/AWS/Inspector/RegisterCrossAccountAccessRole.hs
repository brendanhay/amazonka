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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers the IAM role that grants Amazon Inspector access to AWS Services needed to perform security assessments.
--
--
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
    ) where

import Network.AWS.Inspector.Types
import Network.AWS.Inspector.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'registerCrossAccountAccessRole' smart constructor.
newtype RegisterCrossAccountAccessRole = RegisterCrossAccountAccessRole'
  { _rcaarRoleARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterCrossAccountAccessRole' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcaarRoleARN' - The ARN of the IAM role that grants Amazon Inspector access to AWS Services needed to perform security assessments.
registerCrossAccountAccessRole
    :: Text -- ^ 'rcaarRoleARN'
    -> RegisterCrossAccountAccessRole
registerCrossAccountAccessRole pRoleARN_ =
  RegisterCrossAccountAccessRole' {_rcaarRoleARN = pRoleARN_}


-- | The ARN of the IAM role that grants Amazon Inspector access to AWS Services needed to perform security assessments.
rcaarRoleARN :: Lens' RegisterCrossAccountAccessRole Text
rcaarRoleARN = lens _rcaarRoleARN (\ s a -> s{_rcaarRoleARN = a})

instance AWSRequest RegisterCrossAccountAccessRole
         where
        type Rs RegisterCrossAccountAccessRole =
             RegisterCrossAccountAccessRoleResponse
        request = postJSON inspector
        response
          = receiveNull RegisterCrossAccountAccessRoleResponse'

instance Hashable RegisterCrossAccountAccessRole
         where

instance NFData RegisterCrossAccountAccessRole where

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
data RegisterCrossAccountAccessRoleResponse =
  RegisterCrossAccountAccessRoleResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterCrossAccountAccessRoleResponse' with the minimum fields required to make a request.
--
registerCrossAccountAccessRoleResponse
    :: RegisterCrossAccountAccessRoleResponse
registerCrossAccountAccessRoleResponse = RegisterCrossAccountAccessRoleResponse'


instance NFData
           RegisterCrossAccountAccessRoleResponse
         where
