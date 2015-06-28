{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.RDS.CreateOptionGroup
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a new option group. You can create up to 20 option groups.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateOptionGroup.html>
module Network.AWS.RDS.CreateOptionGroup
    (
    -- * Request
      CreateOptionGroup
    -- ** Request constructor
    , createOptionGroup
    -- ** Request lenses
    , cogTags
    , cogOptionGroupName
    , cogEngineName
    , cogMajorEngineVersion
    , cogOptionGroupDescription

    -- * Response
    , CreateOptionGroupResponse
    -- ** Response constructor
    , createOptionGroupResponse
    -- ** Response lenses
    , creaOptionGroup
    , creaStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'createOptionGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cogTags'
--
-- * 'cogOptionGroupName'
--
-- * 'cogEngineName'
--
-- * 'cogMajorEngineVersion'
--
-- * 'cogOptionGroupDescription'
data CreateOptionGroup = CreateOptionGroup'
    { _cogTags                   :: !(Maybe [Tag])
    , _cogOptionGroupName        :: !Text
    , _cogEngineName             :: !Text
    , _cogMajorEngineVersion     :: !Text
    , _cogOptionGroupDescription :: !Text
    } deriving (Eq,Read,Show)

-- | 'CreateOptionGroup' smart constructor.
createOptionGroup :: Text -> Text -> Text -> Text -> CreateOptionGroup
createOptionGroup pOptionGroupName pEngineName pMajorEngineVersion pOptionGroupDescription =
    CreateOptionGroup'
    { _cogTags = Nothing
    , _cogOptionGroupName = pOptionGroupName
    , _cogEngineName = pEngineName
    , _cogMajorEngineVersion = pMajorEngineVersion
    , _cogOptionGroupDescription = pOptionGroupDescription
    }

-- | FIXME: Undocumented member.
cogTags :: Lens' CreateOptionGroup [Tag]
cogTags = lens _cogTags (\ s a -> s{_cogTags = a}) . _Default;

-- | Specifies the name of the option group to be created.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
--
-- Example: @myoptiongroup@
cogOptionGroupName :: Lens' CreateOptionGroup Text
cogOptionGroupName = lens _cogOptionGroupName (\ s a -> s{_cogOptionGroupName = a});

-- | Specifies the name of the engine that this option group should be
-- associated with.
cogEngineName :: Lens' CreateOptionGroup Text
cogEngineName = lens _cogEngineName (\ s a -> s{_cogEngineName = a});

-- | Specifies the major version of the engine that this option group should
-- be associated with.
cogMajorEngineVersion :: Lens' CreateOptionGroup Text
cogMajorEngineVersion = lens _cogMajorEngineVersion (\ s a -> s{_cogMajorEngineVersion = a});

-- | The description of the option group.
cogOptionGroupDescription :: Lens' CreateOptionGroup Text
cogOptionGroupDescription = lens _cogOptionGroupDescription (\ s a -> s{_cogOptionGroupDescription = a});

instance AWSRequest CreateOptionGroup where
        type Sv CreateOptionGroup = RDS
        type Rs CreateOptionGroup = CreateOptionGroupResponse
        request = post
        response
          = receiveXMLWrapper "CreateOptionGroupResult"
              (\ s h x ->
                 CreateOptionGroupResponse' <$>
                   (x .@? "OptionGroup") <*> (pure s))

instance ToHeaders CreateOptionGroup where
        toHeaders = const mempty

instance ToPath CreateOptionGroup where
        toPath = const "/"

instance ToQuery CreateOptionGroup where
        toQuery CreateOptionGroup'{..}
          = mconcat
              ["Action" =: ("CreateOptionGroup" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Tags" =: toQuery (toQueryList "Tag" <$> _cogTags),
               "OptionGroupName" =: _cogOptionGroupName,
               "EngineName" =: _cogEngineName,
               "MajorEngineVersion" =: _cogMajorEngineVersion,
               "OptionGroupDescription" =:
                 _cogOptionGroupDescription]

-- | /See:/ 'createOptionGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'creaOptionGroup'
--
-- * 'creaStatus'
data CreateOptionGroupResponse = CreateOptionGroupResponse'
    { _creaOptionGroup :: !(Maybe OptionGroup)
    , _creaStatus      :: !Status
    } deriving (Eq,Show)

-- | 'CreateOptionGroupResponse' smart constructor.
createOptionGroupResponse :: Status -> CreateOptionGroupResponse
createOptionGroupResponse pStatus =
    CreateOptionGroupResponse'
    { _creaOptionGroup = Nothing
    , _creaStatus = pStatus
    }

-- | FIXME: Undocumented member.
creaOptionGroup :: Lens' CreateOptionGroupResponse (Maybe OptionGroup)
creaOptionGroup = lens _creaOptionGroup (\ s a -> s{_creaOptionGroup = a});

-- | FIXME: Undocumented member.
creaStatus :: Lens' CreateOptionGroupResponse Status
creaStatus = lens _creaStatus (\ s a -> s{_creaStatus = a});
