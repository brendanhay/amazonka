{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.V2012_01_25.DeprecateWorkflowType
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
-- This operation is eventually consistent. The results are best effort and
-- may not exactly reflect recent updates and changes. Access Control You can
-- use IAM policies to control this action's access to Amazon SWF resources as
-- follows: Use a Resource element with the domain name to limit the action to
-- only specified domains. Use an Action element to allow or deny permission
-- to call this action. Constrain the following parameters by using a
-- Condition element with the appropriate keys. workflowType.name: String
-- constraint. The key is swf:workflowType.name. workflowType.version: String
-- constraint. The key is swf:workflowType.version. If the caller does not
-- have sufficient permissions to invoke the action, or the parameter values
-- fall outside the specified constraints, the action fails by throwing
-- OperationNotPermitted. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows. DeprecateWorkflowType Example
-- POST / HTTP/1.1 Host: swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0
-- (Windows; U; Windows NT 6.1; en-US; rv:1.9.2.25) Gecko/20111212
-- Firefox/3.6.25 ( .NET CLR 3.5.30729; .NET4.0E) Accept: application/json,
-- text/javascript, */* Accept-Language: en-us,en;q=0.5 Accept-Encoding:
-- gzip,deflate Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115
-- Connection: keep-alive Content-Type: application/x-amz-json-1.0
-- X-Requested-With: XMLHttpRequest X-Amz-Date: Mon, 16 Jan 2012 05:04:47 GMT
-- X-Amz-Target: SimpleWorkflowService.DeprecateWorkflowType Content-Encoding:
-- amz-1.0 X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=BGrr1djQvp+YLq3ci2ffpK8KWhZm/PakBL2fFhc3zds=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 102 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "workflowType": {"name": "customerOrderWorkflow", "version":
-- "1.0"} } HTTP/1.1 200 OK Content-Length: 0 Content-Type: application/json
-- x-amzn-RequestId: 9c8d6d3b-3fff-11e1-9e8f-57bb03e21482.
module Network.AWS.SWF.V2012_01_25.DeprecateWorkflowType
    (
    -- * Request
      DeprecateWorkflowType
    -- ** Request constructor
    , mkDeprecateWorkflowTypeInput
    -- ** Request lenses
    , dwtiDomain
    , dwtiWorkflowType

    -- * Response
    , DeprecateWorkflowTypeResponse
    ) where

import           Network.AWS.SWF.V2012_01_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeprecateWorkflowType' request.
mkDeprecateWorkflowTypeInput :: Text -- ^ 'dwtiDomain'
                             -> WorkflowType -- ^ 'dwtiWorkflowType'
                             -> DeprecateWorkflowType
mkDeprecateWorkflowTypeInput p1 p2 = DeprecateWorkflowType
    { _dwtiDomain = p1
    , _dwtiWorkflowType = p2
    }
{-# INLINE mkDeprecateWorkflowTypeInput #-}

data DeprecateWorkflowType = DeprecateWorkflowType
    { _dwtiDomain :: Text
      -- ^ The name of the domain in which the workflow type is registered.
    , _dwtiWorkflowType :: WorkflowType
      -- ^ The workflow type to deprecate.
    } deriving (Show, Generic)

-- | The name of the domain in which the workflow type is registered.
dwtiDomain :: Lens' DeprecateWorkflowType (Text)
dwtiDomain = lens _dwtiDomain (\s a -> s { _dwtiDomain = a })
{-# INLINE dwtiDomain #-}

-- | The workflow type to deprecate.
dwtiWorkflowType :: Lens' DeprecateWorkflowType (WorkflowType)
dwtiWorkflowType = lens _dwtiWorkflowType (\s a -> s { _dwtiWorkflowType = a })
{-# INLINE dwtiWorkflowType #-}

instance ToPath DeprecateWorkflowType

instance ToQuery DeprecateWorkflowType

instance ToHeaders DeprecateWorkflowType

instance ToJSON DeprecateWorkflowType

data DeprecateWorkflowTypeResponse = DeprecateWorkflowTypeResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeprecateWorkflowType where
    type Sv DeprecateWorkflowType = SWF
    type Rs DeprecateWorkflowType = DeprecateWorkflowTypeResponse

    request = get
    response _ = nullaryResponse DeprecateWorkflowTypeResponse
