{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFormation.V2010_05_15.GetTemplate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the template body for a specified stack. You can get the template
-- for running or deleted stacks. For deleted stacks, GetTemplate returns the
-- template for up to 90 days after the stack has been deleted. If the
-- template does not exist, a ValidationError is returned.
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=GetTemplate
-- &StackName=MyStack &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2010-07-27T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature] "{ "AWSTemplateFormatVersion" : "2010-09-09",
-- "Description" : "Simple example", "Resources" : { "MySQS" : { "Type" :
-- "AWS::SQS::Queue", "Properties" : { } } } }.
module Network.AWS.CloudFormation.V2010_05_15.GetTemplate
    (
    -- * Request
      GetTemplate
    -- ** Request constructor
    , getTemplate
    -- ** Request lenses
    , gtiStackName

    -- * Response
    , GetTemplateResponse
    -- ** Response lenses
    , gtoTemplateBody
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudFormation.V2010_05_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetTemplate' request.
getTemplate :: Text -- ^ 'gtiStackName'
            -> GetTemplate
getTemplate p1 = GetTemplate
    { _gtiStackName = p1
    }

data GetTemplate = GetTemplate
    { _gtiStackName :: Text
      -- ^ The name or the unique identifier associated with the stack,
      -- which are not always interchangeable: Running stacks: You can
      -- specify either the stack's name or its unique stack ID. Deleted
      -- stacks: You must specify the unique stack ID. Default: There is
      -- no default value.
    } deriving (Show, Generic)

-- | The name or the unique identifier associated with the stack, which are not
-- always interchangeable: Running stacks: You can specify either the stack's
-- name or its unique stack ID. Deleted stacks: You must specify the unique
-- stack ID. Default: There is no default value.
gtiStackName
    :: Functor f
    => (Text
    -> f (Text))
    -> GetTemplate
    -> f GetTemplate
gtiStackName f x =
    (\y -> x { _gtiStackName = y })
       <$> f (_gtiStackName x)
{-# INLINE gtiStackName #-}

instance ToQuery GetTemplate where
    toQuery = genericQuery def

data GetTemplateResponse = GetTemplateResponse
    { _gtoTemplateBody :: Maybe Text
      -- ^ Structure containing the template body. (For more information, go
      -- to Template Anatomy in the AWS CloudFormation User Guide.).
    } deriving (Show, Generic)

-- | Structure containing the template body. (For more information, go to
-- Template Anatomy in the AWS CloudFormation User Guide.).
gtoTemplateBody
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetTemplateResponse
    -> f GetTemplateResponse
gtoTemplateBody f x =
    (\y -> x { _gtoTemplateBody = y })
       <$> f (_gtoTemplateBody x)
{-# INLINE gtoTemplateBody #-}

instance FromXML GetTemplateResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetTemplate where
    type Sv GetTemplate = CloudFormation
    type Rs GetTemplate = GetTemplateResponse

    request = post "GetTemplate"
    response _ = xmlResponse
