{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.DescribeWorkflowType
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns information about the specified /workflow type/. This includes
-- configuration settings specified when the type was registered and other
-- information such as creation date, current status, etc.
--
-- Access Control
--
-- You can use IAM policies to control this action's access to Amazon SWF
-- resources as follows:
--
-- Use a 'Resource' element with the domain name to limit the action to only
-- specified domains. Use an 'Action' element to allow or deny permission to call
-- this action. Constrain the following parameters by using a 'Condition' element
-- with the appropriate keys.   'workflowType.name': String constraint. The key is 'swf:workflowType.name'.  'workflowType.version': String constraint. The key is 'swf:workflowType.version'.    If the caller does not have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the action
-- fails by throwing 'OperationNotPermitted'. For details and example IAM
-- policies, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_DescribeWorkflowType.html>
module Network.AWS.SWF.DescribeWorkflowType
    (
    -- * Request
      DescribeWorkflowType
    -- ** Request constructor
    , describeWorkflowType
    -- ** Request lenses
    , dwtDomain
    , dwtWorkflowType

    -- * Response
    , DescribeWorkflowTypeResponse
    -- ** Response constructor
    , describeWorkflowTypeResponse
    -- ** Response lenses
    , dwtrConfiguration
    , dwtrTypeInfo
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.SWF.Types
import qualified GHC.Exts

data DescribeWorkflowType = DescribeWorkflowType
    { _dwtDomain       :: Text
    , _dwtWorkflowType :: WorkflowType
    } deriving (Eq, Show)

-- | 'DescribeWorkflowType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dwtDomain' @::@ 'Text'
--
-- * 'dwtWorkflowType' @::@ 'WorkflowType'
--
describeWorkflowType :: Text -- ^ 'dwtDomain'
                     -> WorkflowType -- ^ 'dwtWorkflowType'
                     -> DescribeWorkflowType
describeWorkflowType p1 p2 = DescribeWorkflowType
    { _dwtDomain       = p1
    , _dwtWorkflowType = p2
    }

-- | The name of the domain in which this workflow type is registered.
dwtDomain :: Lens' DescribeWorkflowType Text
dwtDomain = lens _dwtDomain (\s a -> s { _dwtDomain = a })

-- | The workflow type to describe.
dwtWorkflowType :: Lens' DescribeWorkflowType WorkflowType
dwtWorkflowType = lens _dwtWorkflowType (\s a -> s { _dwtWorkflowType = a })

data DescribeWorkflowTypeResponse = DescribeWorkflowTypeResponse
    { _dwtrConfiguration :: WorkflowTypeConfiguration
    , _dwtrTypeInfo      :: WorkflowTypeInfo
    } deriving (Eq, Show)

-- | 'DescribeWorkflowTypeResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dwtrConfiguration' @::@ 'WorkflowTypeConfiguration'
--
-- * 'dwtrTypeInfo' @::@ 'WorkflowTypeInfo'
--
describeWorkflowTypeResponse :: WorkflowTypeInfo -- ^ 'dwtrTypeInfo'
                             -> WorkflowTypeConfiguration -- ^ 'dwtrConfiguration'
                             -> DescribeWorkflowTypeResponse
describeWorkflowTypeResponse p1 p2 = DescribeWorkflowTypeResponse
    { _dwtrTypeInfo      = p1
    , _dwtrConfiguration = p2
    }

-- | Configuration settings of the workflow type registered through 'RegisterWorkflowType'
dwtrConfiguration :: Lens' DescribeWorkflowTypeResponse WorkflowTypeConfiguration
dwtrConfiguration =
    lens _dwtrConfiguration (\s a -> s { _dwtrConfiguration = a })

-- | General information about the workflow type.
--
-- The status of the workflow type (returned in the WorkflowTypeInfo
-- structure) can be one of the following.
--
-- REGISTERED: The type is registered and available. Workers supporting this
-- type should be running.   DEPRECATED: The type was deprecated using 'DeprecateWorkflowType', but is still in use. You should keep workers supporting this type running.
-- You cannot create new workflow executions of this type.
dwtrTypeInfo :: Lens' DescribeWorkflowTypeResponse WorkflowTypeInfo
dwtrTypeInfo = lens _dwtrTypeInfo (\s a -> s { _dwtrTypeInfo = a })

instance ToPath DescribeWorkflowType where
    toPath = const "/"

instance ToQuery DescribeWorkflowType where
    toQuery = const mempty

instance ToHeaders DescribeWorkflowType

instance ToJSON DescribeWorkflowType where
    toJSON DescribeWorkflowType{..} = object
        [ "domain"       .= _dwtDomain
        , "workflowType" .= _dwtWorkflowType
        ]

instance AWSRequest DescribeWorkflowType where
    type Sv DescribeWorkflowType = SWF
    type Rs DescribeWorkflowType = DescribeWorkflowTypeResponse

    request  = post "DescribeWorkflowType"
    response = jsonResponse

instance FromJSON DescribeWorkflowTypeResponse where
    parseJSON = withObject "DescribeWorkflowTypeResponse" $ \o -> DescribeWorkflowTypeResponse
        <$> o .:  "configuration"
        <*> o .:  "typeInfo"
