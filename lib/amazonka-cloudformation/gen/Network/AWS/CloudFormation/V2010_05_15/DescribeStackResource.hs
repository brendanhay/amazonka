{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFormation.V2010_05_15.DescribeStackResource
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a description of the specified resource in the specified stack. For
-- deleted stacks, DescribeStackResource returns resource information for up
-- to 90 days after the stack has been deleted.
-- https://cloudformation.us-east-1.amazonaws.com/
-- ?Action=DescribeStackResource &StackName=MyStack
-- &LogicalResourceId=MyDBInstance &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2011-07-08T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature]
-- arn:aws:cloudformation:us-east-1:123456789:stack/MyStack/aaf549a0-a413-11df-adb3-5081b3858e83
-- MyStack MyDBInstance MyStack_DB1 AWS::RDS::DBInstance 2011-07-07T22:27:28Z
-- CREATE_COMPLETE.
module Network.AWS.CloudFormation.V2010_05_15.DescribeStackResource where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.V2010_05_15.Types
import Network.AWS.Prelude

data DescribeStackResource = DescribeStackResource
    { _dsrjLogicalResourceId :: Text
      -- ^ The logical name of the resource as specified in the template.
      -- Default: There is no default value.
    , _dsrjStackName :: Text
      -- ^ The name or the unique identifier associated with the stack,
      -- which are not always interchangeable: Running stacks: You can
      -- specify either the stack's name or its unique stack ID. Deleted
      -- stacks: You must specify the unique stack ID. Default: There is
      -- no default value.
    } deriving (Show, Generic)

makeLenses ''DescribeStackResource

instance ToQuery DescribeStackResource where
    toQuery = genericQuery def

data DescribeStackResourceResponse = DescribeStackResourceResponse
    { _dsrpStackResourceDetail :: Maybe StackResourceDetail
      -- ^ A StackResourceDetail structure containing the description of the
      -- specified resource in the specified stack.
    } deriving (Show, Generic)

makeLenses ''DescribeStackResourceResponse

instance FromXML DescribeStackResourceResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeStackResource where
    type Sv DescribeStackResource = CloudFormation
    type Rs DescribeStackResource = DescribeStackResourceResponse

    request = post "DescribeStackResource"
    response _ = xmlResponse
