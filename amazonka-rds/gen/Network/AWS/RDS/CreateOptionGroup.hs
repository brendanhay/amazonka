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
    , cogrqTags
    , cogrqOptionGroupName
    , cogrqEngineName
    , cogrqMajorEngineVersion
    , cogrqOptionGroupDescription

    -- * Response
    , CreateOptionGroupResponse
    -- ** Response constructor
    , createOptionGroupResponse
    -- ** Response lenses
    , cogrsOptionGroup
    , cogrsStatus
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
-- * 'cogrqTags'
--
-- * 'cogrqOptionGroupName'
--
-- * 'cogrqEngineName'
--
-- * 'cogrqMajorEngineVersion'
--
-- * 'cogrqOptionGroupDescription'
data CreateOptionGroup = CreateOptionGroup'
    { _cogrqTags                   :: !(Maybe [Tag])
    , _cogrqOptionGroupName        :: !Text
    , _cogrqEngineName             :: !Text
    , _cogrqMajorEngineVersion     :: !Text
    , _cogrqOptionGroupDescription :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateOptionGroup' smart constructor.
createOptionGroup :: Text -> Text -> Text -> Text -> CreateOptionGroup
createOptionGroup pOptionGroupName pEngineName pMajorEngineVersion pOptionGroupDescription =
    CreateOptionGroup'
    { _cogrqTags = Nothing
    , _cogrqOptionGroupName = pOptionGroupName
    , _cogrqEngineName = pEngineName
    , _cogrqMajorEngineVersion = pMajorEngineVersion
    , _cogrqOptionGroupDescription = pOptionGroupDescription
    }

-- | FIXME: Undocumented member.
cogrqTags :: Lens' CreateOptionGroup [Tag]
cogrqTags = lens _cogrqTags (\ s a -> s{_cogrqTags = a}) . _Default;

-- | Specifies the name of the option group to be created.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
--
-- Example: @myoptiongroup@
cogrqOptionGroupName :: Lens' CreateOptionGroup Text
cogrqOptionGroupName = lens _cogrqOptionGroupName (\ s a -> s{_cogrqOptionGroupName = a});

-- | Specifies the name of the engine that this option group should be
-- associated with.
cogrqEngineName :: Lens' CreateOptionGroup Text
cogrqEngineName = lens _cogrqEngineName (\ s a -> s{_cogrqEngineName = a});

-- | Specifies the major version of the engine that this option group should
-- be associated with.
cogrqMajorEngineVersion :: Lens' CreateOptionGroup Text
cogrqMajorEngineVersion = lens _cogrqMajorEngineVersion (\ s a -> s{_cogrqMajorEngineVersion = a});

-- | The description of the option group.
cogrqOptionGroupDescription :: Lens' CreateOptionGroup Text
cogrqOptionGroupDescription = lens _cogrqOptionGroupDescription (\ s a -> s{_cogrqOptionGroupDescription = a});

instance AWSRequest CreateOptionGroup where
        type Sv CreateOptionGroup = RDS
        type Rs CreateOptionGroup = CreateOptionGroupResponse
        request = post
        response
          = receiveXMLWrapper "CreateOptionGroupResult"
              (\ s h x ->
                 CreateOptionGroupResponse' <$>
                   (x .@? "OptionGroup") <*> (pure (fromEnum s)))

instance ToHeaders CreateOptionGroup where
        toHeaders = const mempty

instance ToPath CreateOptionGroup where
        toPath = const "/"

instance ToQuery CreateOptionGroup where
        toQuery CreateOptionGroup'{..}
          = mconcat
              ["Action" =: ("CreateOptionGroup" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Tags" =: toQuery (toQueryList "Tag" <$> _cogrqTags),
               "OptionGroupName" =: _cogrqOptionGroupName,
               "EngineName" =: _cogrqEngineName,
               "MajorEngineVersion" =: _cogrqMajorEngineVersion,
               "OptionGroupDescription" =:
                 _cogrqOptionGroupDescription]

-- | /See:/ 'createOptionGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cogrsOptionGroup'
--
-- * 'cogrsStatus'
data CreateOptionGroupResponse = CreateOptionGroupResponse'
    { _cogrsOptionGroup :: !(Maybe OptionGroup)
    , _cogrsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateOptionGroupResponse' smart constructor.
createOptionGroupResponse :: Int -> CreateOptionGroupResponse
createOptionGroupResponse pStatus =
    CreateOptionGroupResponse'
    { _cogrsOptionGroup = Nothing
    , _cogrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
cogrsOptionGroup :: Lens' CreateOptionGroupResponse (Maybe OptionGroup)
cogrsOptionGroup = lens _cogrsOptionGroup (\ s a -> s{_cogrsOptionGroup = a});

-- | FIXME: Undocumented member.
cogrsStatus :: Lens' CreateOptionGroupResponse Int
cogrsStatus = lens _cogrsStatus (\ s a -> s{_cogrsStatus = a});
