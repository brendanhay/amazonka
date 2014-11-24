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

-- Module      : Network.AWS.SWF.RegisterDomain
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Registers a new domain. Access Control You can use IAM policies to control
-- this action's access to Amazon SWF resources as follows: You cannot use an
-- IAM policy to control domain access for this action. The name of the domain
-- being registered is available as the resource of this action. Use an
-- @Action@ element to allow or deny permission to call this action. You
-- cannot use an IAM policy to constrain this action's parameters. If the
-- caller does not have sufficient permissions to invoke the action, or the
-- parameter values fall outside the specified constraints, the action fails
-- by throwing @OperationNotPermitted@. For details and example IAM policies,
-- see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html
-- Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_RegisterDomain.html>
module Network.AWS.SWF.RegisterDomain
    (
    -- * Request
      RegisterDomain
    -- ** Request constructor
    , registerDomain
    -- ** Request lenses
    , rdDescription
    , rdName
    , rdWorkflowExecutionRetentionPeriodInDays

    -- * Response
    , RegisterDomainResponse
    -- ** Response constructor
    , registerDomainResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.SWF.Types
import qualified GHC.Exts

data RegisterDomain = RegisterDomain
    { _rdDescription                            :: Maybe Text
    , _rdName                                   :: Text
    , _rdWorkflowExecutionRetentionPeriodInDays :: Text
    } deriving (Eq, Ord, Show)

-- | 'RegisterDomain' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdDescription' @::@ 'Maybe' 'Text'
--
-- * 'rdName' @::@ 'Text'
--
-- * 'rdWorkflowExecutionRetentionPeriodInDays' @::@ 'Text'
--
registerDomain :: Text -- ^ 'rdName'
               -> Text -- ^ 'rdWorkflowExecutionRetentionPeriodInDays'
               -> RegisterDomain
registerDomain p1 p2 = RegisterDomain
    { _rdName                                   = p1
    , _rdWorkflowExecutionRetentionPeriodInDays = p2
    , _rdDescription                            = Nothing
    }

-- | Textual description of the domain.
rdDescription :: Lens' RegisterDomain (Maybe Text)
rdDescription = lens _rdDescription (\s a -> s { _rdDescription = a })

-- | Name of the domain to register. The name must be unique. The specified
-- string must not start or end with whitespace. It must not contain a @:@
-- (colon), @/@ (slash), @|@ (vertical bar), or any control characters
-- (\u0000-\u001f | \u007f - \u009f). Also, it must not contain the literal
-- string "arn".
rdName :: Lens' RegisterDomain Text
rdName = lens _rdName (\s a -> s { _rdName = a })

-- | Specifies the duration--/in days/--for which the record (including the
-- history) of workflow executions in this domain should be kept by the
-- service. After the retention period, the workflow execution will not be
-- available in the results of visibility calls. If a duration of @NONE@ is
-- specified, the records for workflow executions in this domain are not
-- retained at all.
rdWorkflowExecutionRetentionPeriodInDays :: Lens' RegisterDomain Text
rdWorkflowExecutionRetentionPeriodInDays =
    lens _rdWorkflowExecutionRetentionPeriodInDays
        (\s a -> s { _rdWorkflowExecutionRetentionPeriodInDays = a })

data RegisterDomainResponse = RegisterDomainResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'RegisterDomainResponse' constructor.
registerDomainResponse :: RegisterDomainResponse
registerDomainResponse = RegisterDomainResponse

instance ToPath RegisterDomain where
    toPath = const "/"

instance ToQuery RegisterDomain where
    toQuery = const mempty

instance ToHeaders RegisterDomain

instance ToJSON RegisterDomain where
    toJSON RegisterDomain{..} = object
        [ "name"                                   .= _rdName
        , "description"                            .= _rdDescription
        , "workflowExecutionRetentionPeriodInDays" .= _rdWorkflowExecutionRetentionPeriodInDays
        ]

instance AWSRequest RegisterDomain where
    type Sv RegisterDomain = SWF
    type Rs RegisterDomain = RegisterDomainResponse

    request  = post "RegisterDomain"
    response = nullResponse RegisterDomainResponse
