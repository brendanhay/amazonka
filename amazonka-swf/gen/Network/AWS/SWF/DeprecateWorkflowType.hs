{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.SWF.DeprecateWorkflowType
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deprecates the specified workflow type. After a workflow type has been
-- deprecated, you cannot create new executions of that type. Executions that
-- were started before the type was deprecated will continue to run. A
-- deprecated workflow type may still be used when calling visibility actions.
-- Access Control You can use IAM policies to control this action's access to
-- Amazon SWF resources as follows: Use a Resource element with the domain
-- name to limit the action to only specified domains. Use an Action element
-- to allow or deny permission to call this action. Constrain the following
-- parameters by using a Condition element with the appropriate keys.
-- workflowType.name: String constraint. The key is swf:workflowType.name.
-- workflowType.version: String constraint. The key is
-- swf:workflowType.version. If the caller does not have sufficient
-- permissions to invoke the action, or the parameter values fall outside the
-- specified constraints, the action fails by throwing OperationNotPermitted.
-- For details and example IAM policies, see Using IAM to Manage Access to
-- Amazon SWF Workflows.
module Network.AWS.SWF.DeprecateWorkflowType
    (
    -- * Request
      DeprecateWorkflowType
    -- ** Request constructor
    , deprecateWorkflowType
    -- ** Request lenses
    , dwt1Domain
    , dwt1WorkflowType

    -- * Response
    , DeprecateWorkflowTypeResponse
    -- ** Response constructor
    , deprecateWorkflowTypeResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.SWF.Types
import qualified GHC.Exts

data DeprecateWorkflowType = DeprecateWorkflowType
    { _dwt1Domain       :: Text
    , _dwt1WorkflowType :: WorkflowType
    } deriving (Eq, Show, Generic)

-- | 'DeprecateWorkflowType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dwt1Domain' @::@ 'Text'
--
-- * 'dwt1WorkflowType' @::@ 'WorkflowType'
--
deprecateWorkflowType :: Text -- ^ 'dwt1Domain'
                      -> WorkflowType -- ^ 'dwt1WorkflowType'
                      -> DeprecateWorkflowType
deprecateWorkflowType p1 p2 = DeprecateWorkflowType
    { _dwt1Domain       = p1
    , _dwt1WorkflowType = p2
    }

-- | The name of the domain in which the workflow type is registered.
dwt1Domain :: Lens' DeprecateWorkflowType Text
dwt1Domain = lens _dwt1Domain (\s a -> s { _dwt1Domain = a })

-- | The workflow type to deprecate.
dwt1WorkflowType :: Lens' DeprecateWorkflowType WorkflowType
dwt1WorkflowType = lens _dwt1WorkflowType (\s a -> s { _dwt1WorkflowType = a })

instance ToPath DeprecateWorkflowType where
    toPath = const "/"

instance ToQuery DeprecateWorkflowType where
    toQuery = const mempty

instance ToHeaders DeprecateWorkflowType

instance ToBody DeprecateWorkflowType where
    toBody = toBody . encode . _dwt1Domain

data DeprecateWorkflowTypeResponse = DeprecateWorkflowTypeResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeprecateWorkflowTypeResponse' constructor.
deprecateWorkflowTypeResponse :: DeprecateWorkflowTypeResponse
deprecateWorkflowTypeResponse = DeprecateWorkflowTypeResponse

instance AWSRequest DeprecateWorkflowType where
    type Sv DeprecateWorkflowType = SWF
    type Rs DeprecateWorkflowType = DeprecateWorkflowTypeResponse

    request  = post
    response = nullaryResponse DeprecateWorkflowTypeResponse
