{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.V2012_01_25.DeprecateDomain
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deprecates the specified domain. After a domain has been deprecated it
-- cannot be used to create new workflow executions or register new types.
-- However, you can still use visibility actions on this domain. Deprecating a
-- domain also deprecates all activity and workflow types registered in the
-- domain. Executions that were started before the domain was deprecated will
-- continue to run. This operation is eventually consistent. The results are
-- best effort and may not exactly reflect recent updates and changes. Access
-- Control You can use IAM policies to control this action's access to Amazon
-- SWF resources as follows: Use a Resource element with the domain name to
-- limit the action to only specified domains. Use an Action element to allow
-- or deny permission to call this action. You cannot use an IAM policy to
-- constrain this action's parameters. If the caller does not have sufficient
-- permissions to invoke the action, or the parameter values fall outside the
-- specified constraints, the action fails by throwing OperationNotPermitted.
-- For details and example IAM policies, see Using IAM to Manage Access to
-- Amazon SWF Workflows. DeprecateDomain Example POST / HTTP/1.1 Host:
-- swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0 (Windows; U; Windows NT
-- 6.1; en-US; rv:1.9.2.25) Gecko/20111212 Firefox/3.6.25 ( .NET CLR
-- 3.5.30729; .NET4.0E) Accept: application/json, text/javascript, */*
-- Accept-Language: en-us,en;q=0.5 Accept-Encoding: gzip,deflate
-- Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115 Connection:
-- keep-alive Content-Type: application/x-amz-json-1.0 X-Requested-With:
-- XMLHttpRequest X-Amz-Date: Mon, 16 Jan 2012 05:07:47 GMT X-Amz-Target:
-- SimpleWorkflowService.DeprecateDomain Content-Encoding: amz-1.0
-- X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=BkJDtbH9uZvrarqXTkBEYuYHO7PPygRI8ykV29Dz/5M=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 21 Pragma: no-cache Cache-Control: no-cache {"name":
-- "867530901"} HTTP/1.1 200 OK Content-Length: 0 Content-Type:
-- application/json x-amzn-RequestId: 0800c01a-4000-11e1-9914-a356b6ea8bdf.
module Network.AWS.SWF.V2012_01_25.DeprecateDomain where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.SWF.V2012_01_25.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

data DeprecateDomain = DeprecateDomain
    { _ddduName :: Text
      -- ^ The name of the domain to deprecate.
    } deriving (Show, Generic)

makeLenses ''DeprecateDomain

instance ToPath DeprecateDomain

instance ToQuery DeprecateDomain

instance ToHeaders DeprecateDomain

instance ToJSON DeprecateDomain

data DeprecateDomainResponse = DeprecateDomainResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeprecateDomainResponse

instance AWSRequest DeprecateDomain where
    type Sv DeprecateDomain = SWF
    type Rs DeprecateDomain = DeprecateDomainResponse

    request = get
    response _ = nullaryResponse DeprecateDomainResponse
