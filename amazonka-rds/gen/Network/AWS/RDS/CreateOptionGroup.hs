{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateOptionGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new option group. You can create up to 20 option groups.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateOptionGroup.html AWS API Reference> for CreateOptionGroup.
module Network.AWS.RDS.CreateOptionGroup
    (
    -- * Creating a Request
      createOptionGroup
    , CreateOptionGroup
    -- * Request Lenses
    , cogTags
    , cogOptionGroupName
    , cogEngineName
    , cogMajorEngineVersion
    , cogOptionGroupDescription

    -- * Destructuring the Response
    , createOptionGroupResponse
    , CreateOptionGroupResponse
    -- * Response Lenses
    , crsOptionGroup
    , crsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.RDS.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'createOptionGroup' smart constructor.
data CreateOptionGroup = CreateOptionGroup'
    { _cogTags                   :: !(Maybe [Tag])
    , _cogOptionGroupName        :: !Text
    , _cogEngineName             :: !Text
    , _cogMajorEngineVersion     :: !Text
    , _cogOptionGroupDescription :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateOptionGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
createOptionGroup
    :: Text -- ^ 'cogOptionGroupName'
    -> Text -- ^ 'cogEngineName'
    -> Text -- ^ 'cogMajorEngineVersion'
    -> Text -- ^ 'cogOptionGroupDescription'
    -> CreateOptionGroup
createOptionGroup pOptionGroupName_ pEngineName_ pMajorEngineVersion_ pOptionGroupDescription_ =
    CreateOptionGroup'
    { _cogTags = Nothing
    , _cogOptionGroupName = pOptionGroupName_
    , _cogEngineName = pEngineName_
    , _cogMajorEngineVersion = pMajorEngineVersion_
    , _cogOptionGroupDescription = pOptionGroupDescription_
    }

-- | Undocumented member.
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
-- Example: 'myoptiongroup'
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
        type Rs CreateOptionGroup = CreateOptionGroupResponse
        request = postQuery rDS
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
               "Tags" =: toQuery (toQueryList "Tag" <$> _cogTags),
               "OptionGroupName" =: _cogOptionGroupName,
               "EngineName" =: _cogEngineName,
               "MajorEngineVersion" =: _cogMajorEngineVersion,
               "OptionGroupDescription" =:
                 _cogOptionGroupDescription]

-- | /See:/ 'createOptionGroupResponse' smart constructor.
data CreateOptionGroupResponse = CreateOptionGroupResponse'
    { _crsOptionGroup :: !(Maybe OptionGroup)
    , _crsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateOptionGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsOptionGroup'
--
-- * 'crsStatus'
createOptionGroupResponse
    :: Int -- ^ 'crsStatus'
    -> CreateOptionGroupResponse
createOptionGroupResponse pStatus_ =
    CreateOptionGroupResponse'
    { _crsOptionGroup = Nothing
    , _crsStatus = pStatus_
    }

-- | Undocumented member.
crsOptionGroup :: Lens' CreateOptionGroupResponse (Maybe OptionGroup)
crsOptionGroup = lens _crsOptionGroup (\ s a -> s{_crsOptionGroup = a});

-- | The response status code.
crsStatus :: Lens' CreateOptionGroupResponse Int
crsStatus = lens _crsStatus (\ s a -> s{_crsStatus = a});
