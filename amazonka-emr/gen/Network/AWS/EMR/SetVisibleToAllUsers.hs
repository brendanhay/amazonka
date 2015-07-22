{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.SetVisibleToAllUsers
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Sets whether all AWS Identity and Access Management (IAM) users under
-- your account can access the specified job flows. This action works on
-- running job flows. You can also set the visibility of a job flow when
-- you launch it using the @VisibleToAllUsers@ parameter of RunJobFlow. The
-- SetVisibleToAllUsers action can be called only by an IAM user who
-- created the job flow or the AWS account that owns the job flow.
--
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_SetVisibleToAllUsers.html>
module Network.AWS.EMR.SetVisibleToAllUsers
    (
    -- * Request
      SetVisibleToAllUsers
    -- ** Request constructor
    , setVisibleToAllUsers
    -- ** Request lenses
    , svtaurqJobFlowIds
    , svtaurqVisibleToAllUsers

    -- * Response
    , SetVisibleToAllUsersResponse
    -- ** Response constructor
    , setVisibleToAllUsersResponse
    ) where

import           Network.AWS.EMR.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input to the SetVisibleToAllUsers action.
--
-- /See:/ 'setVisibleToAllUsers' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'svtaurqJobFlowIds'
--
-- * 'svtaurqVisibleToAllUsers'
data SetVisibleToAllUsers = SetVisibleToAllUsers'
    { _svtaurqJobFlowIds        :: ![Text]
    , _svtaurqVisibleToAllUsers :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetVisibleToAllUsers' smart constructor.
setVisibleToAllUsers :: Bool -> SetVisibleToAllUsers
setVisibleToAllUsers pVisibleToAllUsers =
    SetVisibleToAllUsers'
    { _svtaurqJobFlowIds = mempty
    , _svtaurqVisibleToAllUsers = pVisibleToAllUsers
    }

-- | Identifiers of the job flows to receive the new visibility setting.
svtaurqJobFlowIds :: Lens' SetVisibleToAllUsers [Text]
svtaurqJobFlowIds = lens _svtaurqJobFlowIds (\ s a -> s{_svtaurqJobFlowIds = a});

-- | Whether the specified job flows are visible to all IAM users of the AWS
-- account associated with the job flow. If this value is set to True, all
-- IAM users of that AWS account can view and, if they have the proper IAM
-- policy permissions set, manage the job flows. If it is set to False,
-- only the IAM user that created a job flow can view and manage it.
svtaurqVisibleToAllUsers :: Lens' SetVisibleToAllUsers Bool
svtaurqVisibleToAllUsers = lens _svtaurqVisibleToAllUsers (\ s a -> s{_svtaurqVisibleToAllUsers = a});

instance AWSRequest SetVisibleToAllUsers where
        type Sv SetVisibleToAllUsers = EMR
        type Rs SetVisibleToAllUsers =
             SetVisibleToAllUsersResponse
        request = postJSON
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
              ["JobFlowIds" .= _svtaurqJobFlowIds,
               "VisibleToAllUsers" .= _svtaurqVisibleToAllUsers]

instance ToPath SetVisibleToAllUsers where
        toPath = const "/"

instance ToQuery SetVisibleToAllUsers where
        toQuery = const mempty

-- | /See:/ 'setVisibleToAllUsersResponse' smart constructor.
data SetVisibleToAllUsersResponse =
    SetVisibleToAllUsersResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetVisibleToAllUsersResponse' smart constructor.
setVisibleToAllUsersResponse :: SetVisibleToAllUsersResponse
setVisibleToAllUsersResponse = SetVisibleToAllUsersResponse'
