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
-- Module      : Network.AWS.EMR.SetVisibleToAllUsers
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets whether all AWS Identity and Access Management (IAM) users under
-- your account can access the specified job flows. This action works on
-- running job flows. You can also set the visibility of a job flow when
-- you launch it using the 'VisibleToAllUsers' parameter of RunJobFlow. The
-- SetVisibleToAllUsers action can be called only by an IAM user who
-- created the job flow or the AWS account that owns the job flow.
--
-- /See:/ <http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_SetVisibleToAllUsers.html AWS API Reference> for SetVisibleToAllUsers.
module Network.AWS.EMR.SetVisibleToAllUsers
    (
    -- * Creating a Request
      setVisibleToAllUsers
    , SetVisibleToAllUsers
    -- * Request Lenses
    , svtauJobFlowIds
    , svtauVisibleToAllUsers

    -- * Destructuring the Response
    , setVisibleToAllUsersResponse
    , SetVisibleToAllUsersResponse
    ) where

import           Network.AWS.EMR.Types
import           Network.AWS.EMR.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input to the SetVisibleToAllUsers action.
--
-- /See:/ 'setVisibleToAllUsers' smart constructor.
data SetVisibleToAllUsers = SetVisibleToAllUsers'
    { _svtauJobFlowIds        :: ![Text]
    , _svtauVisibleToAllUsers :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SetVisibleToAllUsers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'svtauJobFlowIds'
--
-- * 'svtauVisibleToAllUsers'
setVisibleToAllUsers
    :: Bool -- ^ 'svtauVisibleToAllUsers'
    -> SetVisibleToAllUsers
setVisibleToAllUsers pVisibleToAllUsers_ =
    SetVisibleToAllUsers'
    { _svtauJobFlowIds = mempty
    , _svtauVisibleToAllUsers = pVisibleToAllUsers_
    }

-- | Identifiers of the job flows to receive the new visibility setting.
svtauJobFlowIds :: Lens' SetVisibleToAllUsers [Text]
svtauJobFlowIds = lens _svtauJobFlowIds (\ s a -> s{_svtauJobFlowIds = a}) . _Coerce;

-- | Whether the specified job flows are visible to all IAM users of the AWS
-- account associated with the job flow. If this value is set to True, all
-- IAM users of that AWS account can view and, if they have the proper IAM
-- policy permissions set, manage the job flows. If it is set to False,
-- only the IAM user that created a job flow can view and manage it.
svtauVisibleToAllUsers :: Lens' SetVisibleToAllUsers Bool
svtauVisibleToAllUsers = lens _svtauVisibleToAllUsers (\ s a -> s{_svtauVisibleToAllUsers = a});

instance AWSRequest SetVisibleToAllUsers where
        type Rs SetVisibleToAllUsers =
             SetVisibleToAllUsersResponse
        request = postJSON eMR
        response = receiveNull SetVisibleToAllUsersResponse'

instance ToHeaders SetVisibleToAllUsers where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ElasticMapReduce.SetVisibleToAllUsers" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SetVisibleToAllUsers where
        toJSON SetVisibleToAllUsers'{..}
          = object
              (catMaybes
                 [Just ("JobFlowIds" .= _svtauJobFlowIds),
                  Just
                    ("VisibleToAllUsers" .= _svtauVisibleToAllUsers)])

instance ToPath SetVisibleToAllUsers where
        toPath = const "/"

instance ToQuery SetVisibleToAllUsers where
        toQuery = const mempty

-- | /See:/ 'setVisibleToAllUsersResponse' smart constructor.
data SetVisibleToAllUsersResponse =
    SetVisibleToAllUsersResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SetVisibleToAllUsersResponse' with the minimum fields required to make a request.
--
setVisibleToAllUsersResponse
    :: SetVisibleToAllUsersResponse
setVisibleToAllUsersResponse = SetVisibleToAllUsersResponse'
