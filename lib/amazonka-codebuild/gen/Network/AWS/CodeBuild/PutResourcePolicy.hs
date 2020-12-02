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
-- Module      : Network.AWS.CodeBuild.PutResourcePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stores a resource policy for the ARN of a @Project@ or @ReportGroup@ object.
module Network.AWS.CodeBuild.PutResourcePolicy
  ( -- * Creating a Request
    putResourcePolicy,
    PutResourcePolicy,

    -- * Request Lenses
    prpPolicy,
    prpResourceARN,

    -- * Destructuring the Response
    putResourcePolicyResponse,
    PutResourcePolicyResponse,

    -- * Response Lenses
    prprsResourceARN,
    prprsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putResourcePolicy' smart constructor.
data PutResourcePolicy = PutResourcePolicy'
  { _prpPolicy :: !Text,
    _prpResourceARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutResourcePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prpPolicy' - A JSON-formatted resource policy. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/project-sharing.html#project-sharing-share Sharing a Project> and <https://docs.aws.amazon.com/codebuild/latest/userguide/report-groups-sharing.html#report-groups-sharing-share Sharing a Report Group> in the /AWS CodeBuild User Guide/ .
--
-- * 'prpResourceARN' - The ARN of the @Project@ or @ReportGroup@ resource you want to associate with a resource policy.
putResourcePolicy ::
  -- | 'prpPolicy'
  Text ->
  -- | 'prpResourceARN'
  Text ->
  PutResourcePolicy
putResourcePolicy pPolicy_ pResourceARN_ =
  PutResourcePolicy'
    { _prpPolicy = pPolicy_,
      _prpResourceARN = pResourceARN_
    }

-- | A JSON-formatted resource policy. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/project-sharing.html#project-sharing-share Sharing a Project> and <https://docs.aws.amazon.com/codebuild/latest/userguide/report-groups-sharing.html#report-groups-sharing-share Sharing a Report Group> in the /AWS CodeBuild User Guide/ .
prpPolicy :: Lens' PutResourcePolicy Text
prpPolicy = lens _prpPolicy (\s a -> s {_prpPolicy = a})

-- | The ARN of the @Project@ or @ReportGroup@ resource you want to associate with a resource policy.
prpResourceARN :: Lens' PutResourcePolicy Text
prpResourceARN = lens _prpResourceARN (\s a -> s {_prpResourceARN = a})

instance AWSRequest PutResourcePolicy where
  type Rs PutResourcePolicy = PutResourcePolicyResponse
  request = postJSON codeBuild
  response =
    receiveJSON
      ( \s h x ->
          PutResourcePolicyResponse'
            <$> (x .?> "resourceArn") <*> (pure (fromEnum s))
      )

instance Hashable PutResourcePolicy

instance NFData PutResourcePolicy

instance ToHeaders PutResourcePolicy where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeBuild_20161006.PutResourcePolicy" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON PutResourcePolicy where
  toJSON PutResourcePolicy' {..} =
    object
      ( catMaybes
          [ Just ("policy" .= _prpPolicy),
            Just ("resourceArn" .= _prpResourceARN)
          ]
      )

instance ToPath PutResourcePolicy where
  toPath = const "/"

instance ToQuery PutResourcePolicy where
  toQuery = const mempty

-- | /See:/ 'putResourcePolicyResponse' smart constructor.
data PutResourcePolicyResponse = PutResourcePolicyResponse'
  { _prprsResourceARN ::
      !(Maybe Text),
    _prprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutResourcePolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prprsResourceARN' - The ARN of the @Project@ or @ReportGroup@ resource that is associated with a resource policy.
--
-- * 'prprsResponseStatus' - -- | The response status code.
putResourcePolicyResponse ::
  -- | 'prprsResponseStatus'
  Int ->
  PutResourcePolicyResponse
putResourcePolicyResponse pResponseStatus_ =
  PutResourcePolicyResponse'
    { _prprsResourceARN = Nothing,
      _prprsResponseStatus = pResponseStatus_
    }

-- | The ARN of the @Project@ or @ReportGroup@ resource that is associated with a resource policy.
prprsResourceARN :: Lens' PutResourcePolicyResponse (Maybe Text)
prprsResourceARN = lens _prprsResourceARN (\s a -> s {_prprsResourceARN = a})

-- | -- | The response status code.
prprsResponseStatus :: Lens' PutResourcePolicyResponse Int
prprsResponseStatus = lens _prprsResponseStatus (\s a -> s {_prprsResponseStatus = a})

instance NFData PutResourcePolicyResponse
