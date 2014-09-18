{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.CreateOptionGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new option group. You can create up to 20 option groups.
-- https://rds.amazonaws.com/ ?Action=CreateOptionGroup
-- &OptionGroupName=myoptiongroup &EngineName=oracle-se1
-- &MajorEngineVersion=11.2 &OptionGroupDescription=Test option group 11.2
-- myoptiongroup oracle-se1 Test option group
-- b2416a8a-84c9-11e1-a264-0b23c28bc344.
module Network.AWS.RDS.CreateOptionGroup
    (
    -- * Request
      CreateOptionGroup
    -- ** Request constructor
    , createOptionGroup
    -- ** Request lenses
    , cogOptionGroupName
    , cogEngineName
    , cogMajorEngineVersion
    , cogOptionGroupDescription
    , cogTags

    -- * Response
    , CreateOptionGroupResponse
    -- ** Response constructor
    , createOptionGroupResponse
    -- ** Response lenses
    , cogrOptionGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import Network.AWS.Prelude

-- | 
data CreateOptionGroup = CreateOptionGroup
    { _cogOptionGroupName :: Text
    , _cogEngineName :: Text
    , _cogMajorEngineVersion :: Text
    , _cogOptionGroupDescription :: Text
    , _cogTags :: [Tag]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateOptionGroup' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @OptionGroupName ::@ @Text@
--
-- * @EngineName ::@ @Text@
--
-- * @MajorEngineVersion ::@ @Text@
--
-- * @OptionGroupDescription ::@ @Text@
--
-- * @Tags ::@ @[Tag]@
--
createOptionGroup :: Text -- ^ 'cogOptionGroupName'
                    -> Text -- ^ 'cogEngineName'
                    -> Text -- ^ 'cogMajorEngineVersion'
                    -> Text -- ^ 'cogOptionGroupDescription'
                    -> CreateOptionGroup
createOptionGroup p1 p2 p3 p4 = CreateOptionGroup
    { _cogOptionGroupName = p1
    , _cogEngineName = p2
    , _cogMajorEngineVersion = p3
    , _cogOptionGroupDescription = p4
    , _cogTags = mempty
    }

-- | Specifies the name of the option group to be created. Constraints: Must be
-- 1 to 255 alphanumeric characters or hyphens First character must be a
-- letter Cannot end with a hyphen or contain two consecutive hyphens Example:
-- myoptiongroup.
cogOptionGroupName :: Lens' CreateOptionGroup Text
cogOptionGroupName =
    lens _cogOptionGroupName (\s a -> s { _cogOptionGroupName = a })

-- | Specifies the name of the engine that this option group should be
-- associated with.
cogEngineName :: Lens' CreateOptionGroup Text
cogEngineName = lens _cogEngineName (\s a -> s { _cogEngineName = a })

-- | Specifies the major version of the engine that this option group should be
-- associated with.
cogMajorEngineVersion :: Lens' CreateOptionGroup Text
cogMajorEngineVersion =
    lens _cogMajorEngineVersion (\s a -> s { _cogMajorEngineVersion = a })

-- | The description of the option group.
cogOptionGroupDescription :: Lens' CreateOptionGroup Text
cogOptionGroupDescription =
    lens _cogOptionGroupDescription
         (\s a -> s { _cogOptionGroupDescription = a })

-- | A list of tags.
cogTags :: Lens' CreateOptionGroup [Tag]
cogTags = lens _cogTags (\s a -> s { _cogTags = a })

instance ToQuery CreateOptionGroup where
    toQuery = genericQuery def

newtype CreateOptionGroupResponse = CreateOptionGroupResponse
    { _cogrOptionGroup :: Maybe OptionGroup
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateOptionGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @OptionGroup ::@ @Maybe OptionGroup@
--
createOptionGroupResponse :: CreateOptionGroupResponse
createOptionGroupResponse = CreateOptionGroupResponse
    { _cogrOptionGroup = Nothing
    }

-- | 
cogrOptionGroup :: Lens' CreateOptionGroupResponse (Maybe OptionGroup)
cogrOptionGroup = lens _cogrOptionGroup (\s a -> s { _cogrOptionGroup = a })

instance FromXML CreateOptionGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateOptionGroup where
    type Sv CreateOptionGroup = RDS
    type Rs CreateOptionGroup = CreateOptionGroupResponse

    request = post "CreateOptionGroup"
    response _ = xmlResponse
