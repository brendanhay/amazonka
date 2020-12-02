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
-- Module      : Network.AWS.SWF.UndeprecateDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undeprecates a previously deprecated domain. After a domain has been undeprecated it can be used to create new workflow executions or register new types.
--
--
-- __Access Control__
--
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--     * You cannot use an IAM policy to constrain this action's parameters.
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
module Network.AWS.SWF.UndeprecateDomain
  ( -- * Creating a Request
    undeprecateDomain,
    UndeprecateDomain,

    -- * Request Lenses
    udName,

    -- * Destructuring the Response
    undeprecateDomainResponse,
    UndeprecateDomainResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SWF.Types

-- | /See:/ 'undeprecateDomain' smart constructor.
newtype UndeprecateDomain = UndeprecateDomain' {_udName :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UndeprecateDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udName' - The name of the domain of the deprecated workflow type.
undeprecateDomain ::
  -- | 'udName'
  Text ->
  UndeprecateDomain
undeprecateDomain pName_ = UndeprecateDomain' {_udName = pName_}

-- | The name of the domain of the deprecated workflow type.
udName :: Lens' UndeprecateDomain Text
udName = lens _udName (\s a -> s {_udName = a})

instance AWSRequest UndeprecateDomain where
  type Rs UndeprecateDomain = UndeprecateDomainResponse
  request = postJSON swf
  response = receiveNull UndeprecateDomainResponse'

instance Hashable UndeprecateDomain

instance NFData UndeprecateDomain

instance ToHeaders UndeprecateDomain where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("SimpleWorkflowService.UndeprecateDomain" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.0" :: ByteString)
          ]
      )

instance ToJSON UndeprecateDomain where
  toJSON UndeprecateDomain' {..} =
    object (catMaybes [Just ("name" .= _udName)])

instance ToPath UndeprecateDomain where
  toPath = const "/"

instance ToQuery UndeprecateDomain where
  toQuery = const mempty

-- | /See:/ 'undeprecateDomainResponse' smart constructor.
data UndeprecateDomainResponse = UndeprecateDomainResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UndeprecateDomainResponse' with the minimum fields required to make a request.
undeprecateDomainResponse ::
  UndeprecateDomainResponse
undeprecateDomainResponse = UndeprecateDomainResponse'

instance NFData UndeprecateDomainResponse
