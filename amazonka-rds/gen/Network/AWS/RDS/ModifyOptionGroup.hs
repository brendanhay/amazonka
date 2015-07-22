{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ModifyOptionGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing option group.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_ModifyOptionGroup.html>
module Network.AWS.RDS.ModifyOptionGroup
    (
    -- * Request
      ModifyOptionGroup
    -- ** Request constructor
    , modifyOptionGroup
    -- ** Request lenses
    , mogrqOptionsToInclude
    , mogrqOptionsToRemove
    , mogrqApplyImmediately
    , mogrqOptionGroupName

    -- * Response
    , ModifyOptionGroupResponse
    -- ** Response constructor
    , modifyOptionGroupResponse
    -- ** Response lenses
    , mogrsOptionGroup
    , mogrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'modifyOptionGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mogrqOptionsToInclude'
--
-- * 'mogrqOptionsToRemove'
--
-- * 'mogrqApplyImmediately'
--
-- * 'mogrqOptionGroupName'
data ModifyOptionGroup = ModifyOptionGroup'
    { _mogrqOptionsToInclude :: !(Maybe [OptionConfiguration])
    , _mogrqOptionsToRemove  :: !(Maybe [Text])
    , _mogrqApplyImmediately :: !(Maybe Bool)
    , _mogrqOptionGroupName  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyOptionGroup' smart constructor.
modifyOptionGroup :: Text -> ModifyOptionGroup
modifyOptionGroup pOptionGroupName =
    ModifyOptionGroup'
    { _mogrqOptionsToInclude = Nothing
    , _mogrqOptionsToRemove = Nothing
    , _mogrqApplyImmediately = Nothing
    , _mogrqOptionGroupName = pOptionGroupName
    }

-- | Options in this list are added to the option group or, if already
-- present, the specified configuration is used to update the existing
-- configuration.
mogrqOptionsToInclude :: Lens' ModifyOptionGroup [OptionConfiguration]
mogrqOptionsToInclude = lens _mogrqOptionsToInclude (\ s a -> s{_mogrqOptionsToInclude = a}) . _Default;

-- | Options in this list are removed from the option group.
mogrqOptionsToRemove :: Lens' ModifyOptionGroup [Text]
mogrqOptionsToRemove = lens _mogrqOptionsToRemove (\ s a -> s{_mogrqOptionsToRemove = a}) . _Default;

-- | Indicates whether the changes should be applied immediately, or during
-- the next maintenance window for each instance associated with the option
-- group.
mogrqApplyImmediately :: Lens' ModifyOptionGroup (Maybe Bool)
mogrqApplyImmediately = lens _mogrqApplyImmediately (\ s a -> s{_mogrqApplyImmediately = a});

-- | The name of the option group to be modified.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security
-- TDE, cannot be removed from an option group, and that option group
-- cannot be removed from a DB instance once it is associated with a DB
-- instance
mogrqOptionGroupName :: Lens' ModifyOptionGroup Text
mogrqOptionGroupName = lens _mogrqOptionGroupName (\ s a -> s{_mogrqOptionGroupName = a});

instance AWSRequest ModifyOptionGroup where
        type Sv ModifyOptionGroup = RDS
        type Rs ModifyOptionGroup = ModifyOptionGroupResponse
        request = post
        response
          = receiveXMLWrapper "ModifyOptionGroupResult"
              (\ s h x ->
                 ModifyOptionGroupResponse' <$>
                   (x .@? "OptionGroup") <*> (pure (fromEnum s)))

instance ToHeaders ModifyOptionGroup where
        toHeaders = const mempty

instance ToPath ModifyOptionGroup where
        toPath = const "/"

instance ToQuery ModifyOptionGroup where
        toQuery ModifyOptionGroup'{..}
          = mconcat
              ["Action" =: ("ModifyOptionGroup" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "OptionsToInclude" =:
                 toQuery
                   (toQueryList "OptionConfiguration" <$>
                      _mogrqOptionsToInclude),
               "OptionsToRemove" =:
                 toQuery
                   (toQueryList "member" <$> _mogrqOptionsToRemove),
               "ApplyImmediately" =: _mogrqApplyImmediately,
               "OptionGroupName" =: _mogrqOptionGroupName]

-- | /See:/ 'modifyOptionGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mogrsOptionGroup'
--
-- * 'mogrsStatus'
data ModifyOptionGroupResponse = ModifyOptionGroupResponse'
    { _mogrsOptionGroup :: !(Maybe OptionGroup)
    , _mogrsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyOptionGroupResponse' smart constructor.
modifyOptionGroupResponse :: Int -> ModifyOptionGroupResponse
modifyOptionGroupResponse pStatus =
    ModifyOptionGroupResponse'
    { _mogrsOptionGroup = Nothing
    , _mogrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
mogrsOptionGroup :: Lens' ModifyOptionGroupResponse (Maybe OptionGroup)
mogrsOptionGroup = lens _mogrsOptionGroup (\ s a -> s{_mogrsOptionGroup = a});

-- | FIXME: Undocumented member.
mogrsStatus :: Lens' ModifyOptionGroupResponse Int
mogrsStatus = lens _mogrsStatus (\ s a -> s{_mogrsStatus = a});
