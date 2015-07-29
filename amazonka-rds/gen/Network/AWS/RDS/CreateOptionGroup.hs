{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateOptionGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a new option group. You can create up to 20 option groups.
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
    , crsOptionGroup
    , crsStatus
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateOptionGroup' smart constructor.
createOptionGroup :: Text -> Text -> Text -> Text -> CreateOptionGroup
createOptionGroup pOptionGroupName_ pEngineName_ pMajorEngineVersion_ pOptionGroupDescription_ =
    CreateOptionGroup'
    { _cogTags = Nothing
    , _cogOptionGroupName = pOptionGroupName_
    , _cogEngineName = pEngineName_
    , _cogMajorEngineVersion = pMajorEngineVersion_
    , _cogOptionGroupDescription = pOptionGroupDescription_
    }

-- | FIXME: Undocumented member.
cogTags :: Lens' CreateOptionGroup [Tag]
cogTags = lens _cogTags (\ s a -> s{_cogTags = a}) . _Default . _Coerce;

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
        request = postQuery
        response
          = receiveXMLWrapper "CreateOptionGroupResult"
              (\ s h x ->
                 CreateOptionGroupResponse' <$>
                   (x .@? "OptionGroup") <*> (pure (fromEnum s)))

instance ToHeaders CreateOptionGroup where
        toHeaders = const mempty

instance ToPath CreateOptionGroup where
        toPath = const mempty

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
-- * 'crsOptionGroup'
--
-- * 'crsStatus'
data CreateOptionGroupResponse = CreateOptionGroupResponse'
    { _crsOptionGroup :: !(Maybe OptionGroup)
    , _crsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateOptionGroupResponse' smart constructor.
createOptionGroupResponse :: Int -> CreateOptionGroupResponse
createOptionGroupResponse pStatus_ =
    CreateOptionGroupResponse'
    { _crsOptionGroup = Nothing
    , _crsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
crsOptionGroup :: Lens' CreateOptionGroupResponse (Maybe OptionGroup)
crsOptionGroup = lens _crsOptionGroup (\ s a -> s{_crsOptionGroup = a});

-- | FIXME: Undocumented member.
crsStatus :: Lens' CreateOptionGroupResponse Int
crsStatus = lens _crsStatus (\ s a -> s{_crsStatus = a});
