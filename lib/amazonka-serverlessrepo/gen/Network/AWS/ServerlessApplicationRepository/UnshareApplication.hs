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
-- Module      : Network.AWS.ServerlessApplicationRepository.UnshareApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unshares an application from an AWS Organization.
--
--
-- This operation can be called only from the organization's master account.
module Network.AWS.ServerlessApplicationRepository.UnshareApplication
  ( -- * Creating a Request
    unshareApplication,
    UnshareApplication,

    -- * Request Lenses
    uaApplicationId,
    uaOrganizationId,

    -- * Destructuring the Response
    unshareApplicationResponse,
    UnshareApplicationResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServerlessApplicationRepository.Types

-- | /See:/ 'unshareApplication' smart constructor.
data UnshareApplication = UnshareApplication'
  { _uaApplicationId ::
      !Text,
    _uaOrganizationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UnshareApplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaApplicationId' - The Amazon Resource Name (ARN) of the application.
--
-- * 'uaOrganizationId' - The AWS Organization ID to unshare the application from.
unshareApplication ::
  -- | 'uaApplicationId'
  Text ->
  -- | 'uaOrganizationId'
  Text ->
  UnshareApplication
unshareApplication pApplicationId_ pOrganizationId_ =
  UnshareApplication'
    { _uaApplicationId = pApplicationId_,
      _uaOrganizationId = pOrganizationId_
    }

-- | The Amazon Resource Name (ARN) of the application.
uaApplicationId :: Lens' UnshareApplication Text
uaApplicationId = lens _uaApplicationId (\s a -> s {_uaApplicationId = a})

-- | The AWS Organization ID to unshare the application from.
uaOrganizationId :: Lens' UnshareApplication Text
uaOrganizationId = lens _uaOrganizationId (\s a -> s {_uaOrganizationId = a})

instance AWSRequest UnshareApplication where
  type Rs UnshareApplication = UnshareApplicationResponse
  request = postJSON serverlessApplicationRepository
  response = receiveNull UnshareApplicationResponse'

instance Hashable UnshareApplication

instance NFData UnshareApplication

instance ToHeaders UnshareApplication where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UnshareApplication where
  toJSON UnshareApplication' {..} =
    object (catMaybes [Just ("organizationId" .= _uaOrganizationId)])

instance ToPath UnshareApplication where
  toPath UnshareApplication' {..} =
    mconcat ["/applications/", toBS _uaApplicationId, "/unshare"]

instance ToQuery UnshareApplication where
  toQuery = const mempty

-- | /See:/ 'unshareApplicationResponse' smart constructor.
data UnshareApplicationResponse = UnshareApplicationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UnshareApplicationResponse' with the minimum fields required to make a request.
unshareApplicationResponse ::
  UnshareApplicationResponse
unshareApplicationResponse = UnshareApplicationResponse'

instance NFData UnshareApplicationResponse
