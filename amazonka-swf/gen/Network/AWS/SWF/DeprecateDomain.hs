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
-- Module      : Network.AWS.SWF.DeprecateDomain
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprecates the specified domain. After a domain has been deprecated it cannot be used to create new workflow executions or register new types. However, you can still use visibility actions on this domain. Deprecating a domain also deprecates all activity and workflow types registered in the domain. Executions that were started before the domain was deprecated continues to run.
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
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
module Network.AWS.SWF.DeprecateDomain
    (
    -- * Creating a Request
      deprecateDomain
    , DeprecateDomain
    -- * Request Lenses
    , dName

    -- * Destructuring the Response
    , deprecateDomainResponse
    , DeprecateDomainResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SWF.Types
import Network.AWS.SWF.Types.Product

-- | /See:/ 'deprecateDomain' smart constructor.
newtype DeprecateDomain = DeprecateDomain'
  { _dName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeprecateDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dName' - The name of the domain to deprecate.
deprecateDomain
    :: Text -- ^ 'dName'
    -> DeprecateDomain
deprecateDomain pName_ = DeprecateDomain' {_dName = pName_}


-- | The name of the domain to deprecate.
dName :: Lens' DeprecateDomain Text
dName = lens _dName (\ s a -> s{_dName = a})

instance AWSRequest DeprecateDomain where
        type Rs DeprecateDomain = DeprecateDomainResponse
        request = postJSON swf
        response = receiveNull DeprecateDomainResponse'

instance Hashable DeprecateDomain where

instance NFData DeprecateDomain where

instance ToHeaders DeprecateDomain where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SimpleWorkflowService.DeprecateDomain" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON DeprecateDomain where
        toJSON DeprecateDomain'{..}
          = object (catMaybes [Just ("name" .= _dName)])

instance ToPath DeprecateDomain where
        toPath = const "/"

instance ToQuery DeprecateDomain where
        toQuery = const mempty

-- | /See:/ 'deprecateDomainResponse' smart constructor.
data DeprecateDomainResponse =
  DeprecateDomainResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeprecateDomainResponse' with the minimum fields required to make a request.
--
deprecateDomainResponse
    :: DeprecateDomainResponse
deprecateDomainResponse = DeprecateDomainResponse'


instance NFData DeprecateDomainResponse where
