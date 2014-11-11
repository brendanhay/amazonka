{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.CloudFormation.GetTemplate
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
-- template for up to 90 days after the stack has been deleted.
module Network.AWS.CloudFormation.GetTemplate
    (
    -- * Request
      GetTemplateInput
    -- ** Request constructor
    , getTemplateInput
    -- ** Request lenses
    , gtiStackName

    -- * Response
    , GetTemplateOutput
    -- ** Response constructor
    , getTemplateOutput
    -- ** Response lenses
    , gtoTemplateBody
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types

newtype GetTemplateInput = GetTemplateInput
    { _gtiStackName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'GetTemplateInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gtiStackName' @::@ 'Text'
--
getTemplateInput :: Text -- ^ 'gtiStackName'
                 -> GetTemplateInput
getTemplateInput p1 = GetTemplateInput
    { _gtiStackName = p1
    }

-- | The name or the unique identifier associated with the stack, which are
-- not always interchangeable: Running stacks: You can specify either the
-- stack's name or its unique stack ID. Deleted stacks: You must specify the
-- unique stack ID. Default: There is no default value.
gtiStackName :: Lens' GetTemplateInput Text
gtiStackName = lens _gtiStackName (\s a -> s { _gtiStackName = a })
instance ToQuery GetTemplateInput

instance ToPath GetTemplateInput where
    toPath = const "/"

newtype GetTemplateOutput = GetTemplateOutput
    { _gtoTemplateBody :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetTemplateOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gtoTemplateBody' @::@ 'Maybe' 'Text'
--
getTemplateOutput :: GetTemplateOutput
getTemplateOutput = GetTemplateOutput
    { _gtoTemplateBody = Nothing
    }

-- | Structure containing the template body. (For more information, go to
-- Template Anatomy in the AWS CloudFormation User Guide.).
gtoTemplateBody :: Lens' GetTemplateOutput (Maybe Text)
gtoTemplateBody = lens _gtoTemplateBody (\s a -> s { _gtoTemplateBody = a })
instance FromXML GetTemplateOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GetTemplateOutput"

instance AWSRequest GetTemplateInput where
    type Sv GetTemplateInput = CloudFormation
    type Rs GetTemplateInput = GetTemplateOutput

    request  = post "GetTemplate"
    response = xmlResponse $ \h x -> GetTemplateOutput
        <$> x %| "TemplateBody"
