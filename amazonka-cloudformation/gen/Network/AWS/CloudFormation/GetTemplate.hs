{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFormation
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
module Network.AWS.CloudFormation
    (
    -- * Request
      GetTemplate
    -- ** Request constructor
    , mkGetTemplate
    -- ** Request lenses
    , gtStackName

    -- * Response
    , GetTemplateResponse
    -- ** Response constructor
    , mkGetTemplateResponse
    -- ** Response lenses
    , gtrTemplateBody
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types
import Network.AWS.Prelude

-- | The input for a GetTemplate action.
newtype GetTemplate = GetTemplate
    { _gtStackName :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetTemplate' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StackName ::@ @Text@
--
mkGetTemplate :: Text -- ^ 'gtStackName'
              -> GetTemplate
mkGetTemplate p1 = GetTemplate
    { _gtStackName = p1
    }

-- | The name or the unique identifier associated with the stack, which are not
-- always interchangeable: Running stacks: You can specify either the stack's
-- name or its unique stack ID. Deleted stacks: You must specify the unique
-- stack ID. Default: There is no default value.
gtStackName :: Lens' GetTemplate Text
gtStackName = lens _gtStackName (\s a -> s { _gtStackName = a })

instance ToQuery GetTemplate where
    toQuery = genericQuery def

-- | The output for GetTemplate action.
newtype GetTemplateResponse = GetTemplateResponse
    { _gtrTemplateBody :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetTemplateResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TemplateBody ::@ @Maybe Text@
--
mkGetTemplateResponse :: GetTemplateResponse
mkGetTemplateResponse = GetTemplateResponse
    { _gtrTemplateBody = Nothing
    }

-- | Structure containing the template body. (For more information, go to
-- Template Anatomy in the AWS CloudFormation User Guide.).
gtrTemplateBody :: Lens' GetTemplateResponse (Maybe Text)
gtrTemplateBody = lens _gtrTemplateBody (\s a -> s { _gtrTemplateBody = a })

instance FromXML GetTemplateResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetTemplate where
    type Sv GetTemplate = CloudFormation
    type Rs GetTemplate = GetTemplateResponse

    request = post "GetTemplate"
    response _ = xmlResponse
