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
-- Module      : Network.AWS.IAM.PutUserPermissionsBoundary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates the policy that is specified as the IAM user's permissions boundary. You can use an AWS managed policy or a customer managed policy to set the boundary for a user. Use the boundary to control the maximum permissions that the user can have. Setting a permissions boundary is an advanced feature that can affect the permissions for the user.
--
--
-- /Important:/ Policies that are used as permissions boundaries do not provide permissions. You must also attach a permissions policy to the user. To learn how the effective permissions for a user are evaluated, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_evaluation-logic.html IAM JSON Policy Evaluation Logic> in the IAM User Guide.
module Network.AWS.IAM.PutUserPermissionsBoundary
  ( -- * Creating a Request
    putUserPermissionsBoundary,
    PutUserPermissionsBoundary,

    -- * Request Lenses
    pupbUserName,
    pupbPermissionsBoundary,

    -- * Destructuring the Response
    putUserPermissionsBoundaryResponse,
    PutUserPermissionsBoundaryResponse,
  )
where

import Network.AWS.IAM.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putUserPermissionsBoundary' smart constructor.
data PutUserPermissionsBoundary = PutUserPermissionsBoundary'
  { _pupbUserName ::
      !Text,
    _pupbPermissionsBoundary :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutUserPermissionsBoundary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pupbUserName' - The name (friendly name, not ARN) of the IAM user for which you want to set the permissions boundary.
--
-- * 'pupbPermissionsBoundary' - The ARN of the policy that is used to set the permissions boundary for the user.
putUserPermissionsBoundary ::
  -- | 'pupbUserName'
  Text ->
  -- | 'pupbPermissionsBoundary'
  Text ->
  PutUserPermissionsBoundary
putUserPermissionsBoundary pUserName_ pPermissionsBoundary_ =
  PutUserPermissionsBoundary'
    { _pupbUserName = pUserName_,
      _pupbPermissionsBoundary = pPermissionsBoundary_
    }

-- | The name (friendly name, not ARN) of the IAM user for which you want to set the permissions boundary.
pupbUserName :: Lens' PutUserPermissionsBoundary Text
pupbUserName = lens _pupbUserName (\s a -> s {_pupbUserName = a})

-- | The ARN of the policy that is used to set the permissions boundary for the user.
pupbPermissionsBoundary :: Lens' PutUserPermissionsBoundary Text
pupbPermissionsBoundary = lens _pupbPermissionsBoundary (\s a -> s {_pupbPermissionsBoundary = a})

instance AWSRequest PutUserPermissionsBoundary where
  type
    Rs PutUserPermissionsBoundary =
      PutUserPermissionsBoundaryResponse
  request = postQuery iam
  response = receiveNull PutUserPermissionsBoundaryResponse'

instance Hashable PutUserPermissionsBoundary

instance NFData PutUserPermissionsBoundary

instance ToHeaders PutUserPermissionsBoundary where
  toHeaders = const mempty

instance ToPath PutUserPermissionsBoundary where
  toPath = const "/"

instance ToQuery PutUserPermissionsBoundary where
  toQuery PutUserPermissionsBoundary' {..} =
    mconcat
      [ "Action" =: ("PutUserPermissionsBoundary" :: ByteString),
        "Version" =: ("2010-05-08" :: ByteString),
        "UserName" =: _pupbUserName,
        "PermissionsBoundary" =: _pupbPermissionsBoundary
      ]

-- | /See:/ 'putUserPermissionsBoundaryResponse' smart constructor.
data PutUserPermissionsBoundaryResponse = PutUserPermissionsBoundaryResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutUserPermissionsBoundaryResponse' with the minimum fields required to make a request.
putUserPermissionsBoundaryResponse ::
  PutUserPermissionsBoundaryResponse
putUserPermissionsBoundaryResponse =
  PutUserPermissionsBoundaryResponse'

instance NFData PutUserPermissionsBoundaryResponse
