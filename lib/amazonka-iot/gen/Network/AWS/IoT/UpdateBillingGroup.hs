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
-- Module      : Network.AWS.IoT.UpdateBillingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information about the billing group.
module Network.AWS.IoT.UpdateBillingGroup
  ( -- * Creating a Request
    updateBillingGroup,
    UpdateBillingGroup,

    -- * Request Lenses
    ubgExpectedVersion,
    ubgBillingGroupName,
    ubgBillingGroupProperties,

    -- * Destructuring the Response
    updateBillingGroupResponse,
    UpdateBillingGroupResponse,

    -- * Response Lenses
    ubgrsVersion,
    ubgrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateBillingGroup' smart constructor.
data UpdateBillingGroup = UpdateBillingGroup'
  { _ubgExpectedVersion ::
      !(Maybe Integer),
    _ubgBillingGroupName :: !Text,
    _ubgBillingGroupProperties :: !BillingGroupProperties
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateBillingGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ubgExpectedVersion' - The expected version of the billing group. If the version of the billing group does not match the expected version specified in the request, the @UpdateBillingGroup@ request is rejected with a @VersionConflictException@ .
--
-- * 'ubgBillingGroupName' - The name of the billing group.
--
-- * 'ubgBillingGroupProperties' - The properties of the billing group.
updateBillingGroup ::
  -- | 'ubgBillingGroupName'
  Text ->
  -- | 'ubgBillingGroupProperties'
  BillingGroupProperties ->
  UpdateBillingGroup
updateBillingGroup pBillingGroupName_ pBillingGroupProperties_ =
  UpdateBillingGroup'
    { _ubgExpectedVersion = Nothing,
      _ubgBillingGroupName = pBillingGroupName_,
      _ubgBillingGroupProperties = pBillingGroupProperties_
    }

-- | The expected version of the billing group. If the version of the billing group does not match the expected version specified in the request, the @UpdateBillingGroup@ request is rejected with a @VersionConflictException@ .
ubgExpectedVersion :: Lens' UpdateBillingGroup (Maybe Integer)
ubgExpectedVersion = lens _ubgExpectedVersion (\s a -> s {_ubgExpectedVersion = a})

-- | The name of the billing group.
ubgBillingGroupName :: Lens' UpdateBillingGroup Text
ubgBillingGroupName = lens _ubgBillingGroupName (\s a -> s {_ubgBillingGroupName = a})

-- | The properties of the billing group.
ubgBillingGroupProperties :: Lens' UpdateBillingGroup BillingGroupProperties
ubgBillingGroupProperties = lens _ubgBillingGroupProperties (\s a -> s {_ubgBillingGroupProperties = a})

instance AWSRequest UpdateBillingGroup where
  type Rs UpdateBillingGroup = UpdateBillingGroupResponse
  request = patchJSON ioT
  response =
    receiveJSON
      ( \s h x ->
          UpdateBillingGroupResponse'
            <$> (x .?> "version") <*> (pure (fromEnum s))
      )

instance Hashable UpdateBillingGroup

instance NFData UpdateBillingGroup

instance ToHeaders UpdateBillingGroup where
  toHeaders = const mempty

instance ToJSON UpdateBillingGroup where
  toJSON UpdateBillingGroup' {..} =
    object
      ( catMaybes
          [ ("expectedVersion" .=) <$> _ubgExpectedVersion,
            Just ("billingGroupProperties" .= _ubgBillingGroupProperties)
          ]
      )

instance ToPath UpdateBillingGroup where
  toPath UpdateBillingGroup' {..} =
    mconcat ["/billing-groups/", toBS _ubgBillingGroupName]

instance ToQuery UpdateBillingGroup where
  toQuery = const mempty

-- | /See:/ 'updateBillingGroupResponse' smart constructor.
data UpdateBillingGroupResponse = UpdateBillingGroupResponse'
  { _ubgrsVersion ::
      !(Maybe Integer),
    _ubgrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateBillingGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ubgrsVersion' - The latest version of the billing group.
--
-- * 'ubgrsResponseStatus' - -- | The response status code.
updateBillingGroupResponse ::
  -- | 'ubgrsResponseStatus'
  Int ->
  UpdateBillingGroupResponse
updateBillingGroupResponse pResponseStatus_ =
  UpdateBillingGroupResponse'
    { _ubgrsVersion = Nothing,
      _ubgrsResponseStatus = pResponseStatus_
    }

-- | The latest version of the billing group.
ubgrsVersion :: Lens' UpdateBillingGroupResponse (Maybe Integer)
ubgrsVersion = lens _ubgrsVersion (\s a -> s {_ubgrsVersion = a})

-- | -- | The response status code.
ubgrsResponseStatus :: Lens' UpdateBillingGroupResponse Int
ubgrsResponseStatus = lens _ubgrsResponseStatus (\s a -> s {_ubgrsResponseStatus = a})

instance NFData UpdateBillingGroupResponse
