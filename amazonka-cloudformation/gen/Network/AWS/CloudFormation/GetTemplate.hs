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

-- Module      : Network.AWS.CloudFormation.GetTemplate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the template body for a specified stack. You can get the template for
-- running or deleted stacks.
--
-- For deleted stacks, GetTemplate returns the template for up to 90 days after
-- the stack has been deleted.
--
-- If the template does not exist, a 'ValidationError' is returned.
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_GetTemplate.html>
module Network.AWS.CloudFormation.GetTemplate
    (
    -- * Request
      GetTemplate
    -- ** Request constructor
    , getTemplate
    -- ** Request lenses
    , gtStackName

    -- * Response
    , GetTemplateResponse
    -- ** Response constructor
    , getTemplateResponse
    -- ** Response lenses
    , gtrTemplateBody
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types
import qualified GHC.Exts

newtype GetTemplate = GetTemplate
    { _gtStackName :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'GetTemplate' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gtStackName' @::@ 'Text'
--
getTemplate :: Text -- ^ 'gtStackName'
            -> GetTemplate
getTemplate p1 = GetTemplate
    { _gtStackName = p1
    }

-- | The name or the unique identifier associated with the stack, which are not
-- always interchangeable:
--
-- Running stacks: You can specify either the stack's name or its unique stack
-- ID. Deleted stacks: You must specify the unique stack ID.  Default: There is
-- no default value.
--
gtStackName :: Lens' GetTemplate Text
gtStackName = lens _gtStackName (\s a -> s { _gtStackName = a })

newtype GetTemplateResponse = GetTemplateResponse
    { _gtrTemplateBody :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

-- | 'GetTemplateResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gtrTemplateBody' @::@ 'Maybe' 'Text'
--
getTemplateResponse :: GetTemplateResponse
getTemplateResponse = GetTemplateResponse
    { _gtrTemplateBody = Nothing
    }

-- | Structure containing the template body. (For more information, go to <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html TemplateAnatomy> in the AWS CloudFormation User Guide.)
--
gtrTemplateBody :: Lens' GetTemplateResponse (Maybe Text)
gtrTemplateBody = lens _gtrTemplateBody (\s a -> s { _gtrTemplateBody = a })

instance ToPath GetTemplate where
    toPath = const "/"

instance ToQuery GetTemplate where
    toQuery GetTemplate{..} = mconcat
        [ "StackName" =? _gtStackName
        ]

instance ToHeaders GetTemplate

instance AWSRequest GetTemplate where
    type Sv GetTemplate = CloudFormation
    type Rs GetTemplate = GetTemplateResponse

    request  = post "GetTemplate"
    response = xmlResponse

instance FromXML GetTemplateResponse where
    parseXML = withElement "GetTemplateResult" $ \x -> GetTemplateResponse
        <$> x .@? "TemplateBody"
