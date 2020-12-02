{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.SetVisibleToAllUsers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the 'Cluster$VisibleToAllUsers' value, which determines whether the cluster is visible to all IAM users of the AWS account associated with the cluster. Only the IAM user who created the cluster or the AWS account root user can call this action. The default value, @true@ , indicates that all IAM users in the AWS account can perform cluster actions if they have the proper IAM policy permissions. If set to @false@ , only the IAM user that created the cluster can perform actions. This action works on running clusters. You can override the default @true@ setting when you create a cluster by using the @VisibleToAllUsers@ parameter with @RunJobFlow@ .
module Network.AWS.EMR.SetVisibleToAllUsers
  ( -- * Creating a Request
    setVisibleToAllUsers,
    SetVisibleToAllUsers,

    -- * Request Lenses
    svtauJobFlowIds,
    svtauVisibleToAllUsers,

    -- * Destructuring the Response
    setVisibleToAllUsersResponse,
    SetVisibleToAllUsersResponse,
  )
where

import Network.AWS.EMR.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input to the SetVisibleToAllUsers action.
--
--
--
-- /See:/ 'setVisibleToAllUsers' smart constructor.
data SetVisibleToAllUsers = SetVisibleToAllUsers'
  { _svtauJobFlowIds ::
      ![Text],
    _svtauVisibleToAllUsers :: !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SetVisibleToAllUsers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'svtauJobFlowIds' - The unique identifier of the job flow (cluster).
--
-- * 'svtauVisibleToAllUsers' - A value of @true@ indicates that all IAM users in the AWS account can perform cluster actions if they have the proper IAM policy permissions. This is the default. A value of @false@ indicates that only the IAM user who created the cluster can perform actions.
setVisibleToAllUsers ::
  -- | 'svtauVisibleToAllUsers'
  Bool ->
  SetVisibleToAllUsers
setVisibleToAllUsers pVisibleToAllUsers_ =
  SetVisibleToAllUsers'
    { _svtauJobFlowIds = mempty,
      _svtauVisibleToAllUsers = pVisibleToAllUsers_
    }

-- | The unique identifier of the job flow (cluster).
svtauJobFlowIds :: Lens' SetVisibleToAllUsers [Text]
svtauJobFlowIds = lens _svtauJobFlowIds (\s a -> s {_svtauJobFlowIds = a}) . _Coerce

-- | A value of @true@ indicates that all IAM users in the AWS account can perform cluster actions if they have the proper IAM policy permissions. This is the default. A value of @false@ indicates that only the IAM user who created the cluster can perform actions.
svtauVisibleToAllUsers :: Lens' SetVisibleToAllUsers Bool
svtauVisibleToAllUsers = lens _svtauVisibleToAllUsers (\s a -> s {_svtauVisibleToAllUsers = a})

instance AWSRequest SetVisibleToAllUsers where
  type Rs SetVisibleToAllUsers = SetVisibleToAllUsersResponse
  request = postJSON emr
  response = receiveNull SetVisibleToAllUsersResponse'

instance Hashable SetVisibleToAllUsers

instance NFData SetVisibleToAllUsers

instance ToHeaders SetVisibleToAllUsers where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("ElasticMapReduce.SetVisibleToAllUsers" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON SetVisibleToAllUsers where
  toJSON SetVisibleToAllUsers' {..} =
    object
      ( catMaybes
          [ Just ("JobFlowIds" .= _svtauJobFlowIds),
            Just ("VisibleToAllUsers" .= _svtauVisibleToAllUsers)
          ]
      )

instance ToPath SetVisibleToAllUsers where
  toPath = const "/"

instance ToQuery SetVisibleToAllUsers where
  toQuery = const mempty

-- | /See:/ 'setVisibleToAllUsersResponse' smart constructor.
data SetVisibleToAllUsersResponse = SetVisibleToAllUsersResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SetVisibleToAllUsersResponse' with the minimum fields required to make a request.
setVisibleToAllUsersResponse ::
  SetVisibleToAllUsersResponse
setVisibleToAllUsersResponse = SetVisibleToAllUsersResponse'

instance NFData SetVisibleToAllUsersResponse
