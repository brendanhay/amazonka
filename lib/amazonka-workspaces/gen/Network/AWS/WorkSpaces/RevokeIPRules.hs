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
-- Module      : Network.AWS.WorkSpaces.RevokeIPRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more rules from the specified IP access control group.
module Network.AWS.WorkSpaces.RevokeIPRules
  ( -- * Creating a Request
    revokeIPRules,
    RevokeIPRules,

    -- * Request Lenses
    rirGroupId,
    rirUserRules,

    -- * Destructuring the Response
    revokeIPRulesResponse,
    RevokeIPRulesResponse,

    -- * Response Lenses
    rirrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'revokeIPRules' smart constructor.
data RevokeIPRules = RevokeIPRules'
  { _rirGroupId :: !Text,
    _rirUserRules :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RevokeIPRules' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rirGroupId' - The identifier of the group.
--
-- * 'rirUserRules' - The rules to remove from the group.
revokeIPRules ::
  -- | 'rirGroupId'
  Text ->
  RevokeIPRules
revokeIPRules pGroupId_ =
  RevokeIPRules' {_rirGroupId = pGroupId_, _rirUserRules = mempty}

-- | The identifier of the group.
rirGroupId :: Lens' RevokeIPRules Text
rirGroupId = lens _rirGroupId (\s a -> s {_rirGroupId = a})

-- | The rules to remove from the group.
rirUserRules :: Lens' RevokeIPRules [Text]
rirUserRules = lens _rirUserRules (\s a -> s {_rirUserRules = a}) . _Coerce

instance AWSRequest RevokeIPRules where
  type Rs RevokeIPRules = RevokeIPRulesResponse
  request = postJSON workSpaces
  response =
    receiveEmpty
      (\s h x -> RevokeIPRulesResponse' <$> (pure (fromEnum s)))

instance Hashable RevokeIPRules

instance NFData RevokeIPRules

instance ToHeaders RevokeIPRules where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("WorkspacesService.RevokeIpRules" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON RevokeIPRules where
  toJSON RevokeIPRules' {..} =
    object
      ( catMaybes
          [ Just ("GroupId" .= _rirGroupId),
            Just ("UserRules" .= _rirUserRules)
          ]
      )

instance ToPath RevokeIPRules where
  toPath = const "/"

instance ToQuery RevokeIPRules where
  toQuery = const mempty

-- | /See:/ 'revokeIPRulesResponse' smart constructor.
newtype RevokeIPRulesResponse = RevokeIPRulesResponse'
  { _rirrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RevokeIPRulesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rirrsResponseStatus' - -- | The response status code.
revokeIPRulesResponse ::
  -- | 'rirrsResponseStatus'
  Int ->
  RevokeIPRulesResponse
revokeIPRulesResponse pResponseStatus_ =
  RevokeIPRulesResponse' {_rirrsResponseStatus = pResponseStatus_}

-- | -- | The response status code.
rirrsResponseStatus :: Lens' RevokeIPRulesResponse Int
rirrsResponseStatus = lens _rirrsResponseStatus (\s a -> s {_rirrsResponseStatus = a})

instance NFData RevokeIPRulesResponse
