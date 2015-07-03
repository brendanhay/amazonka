{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.RDS.ModifyOptionGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Modifies an existing option group.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_ModifyOptionGroup.html>
module Network.AWS.RDS.ModifyOptionGroup
    (
    -- * Request
      ModifyOptionGroup
    -- ** Request constructor
    , modifyOptionGroup
    -- ** Request lenses
    , mogOptionsToInclude
    , mogOptionsToRemove
    , mogApplyImmediately
    , mogOptionGroupName

    -- * Response
    , ModifyOptionGroupResponse
    -- ** Response constructor
    , modifyOptionGroupResponse
    -- ** Response lenses
    , mogrOptionGroup
    , mogrStatus
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
-- * 'mogOptionsToInclude'
--
-- * 'mogOptionsToRemove'
--
-- * 'mogApplyImmediately'
--
-- * 'mogOptionGroupName'
data ModifyOptionGroup = ModifyOptionGroup'
    { _mogOptionsToInclude :: !(Maybe [OptionConfiguration])
    , _mogOptionsToRemove  :: !(Maybe [Text])
    , _mogApplyImmediately :: !(Maybe Bool)
    , _mogOptionGroupName  :: !Text
    } deriving (Eq,Read,Show)

-- | 'ModifyOptionGroup' smart constructor.
modifyOptionGroup :: Text -> ModifyOptionGroup
modifyOptionGroup pOptionGroupName =
    ModifyOptionGroup'
    { _mogOptionsToInclude = Nothing
    , _mogOptionsToRemove = Nothing
    , _mogApplyImmediately = Nothing
    , _mogOptionGroupName = pOptionGroupName
    }

-- | Options in this list are added to the option group or, if already
-- present, the specified configuration is used to update the existing
-- configuration.
mogOptionsToInclude :: Lens' ModifyOptionGroup [OptionConfiguration]
mogOptionsToInclude = lens _mogOptionsToInclude (\ s a -> s{_mogOptionsToInclude = a}) . _Default;

-- | Options in this list are removed from the option group.
mogOptionsToRemove :: Lens' ModifyOptionGroup [Text]
mogOptionsToRemove = lens _mogOptionsToRemove (\ s a -> s{_mogOptionsToRemove = a}) . _Default;

-- | Indicates whether the changes should be applied immediately, or during
-- the next maintenance window for each instance associated with the option
-- group.
mogApplyImmediately :: Lens' ModifyOptionGroup (Maybe Bool)
mogApplyImmediately = lens _mogApplyImmediately (\ s a -> s{_mogApplyImmediately = a});

-- | The name of the option group to be modified.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security
-- TDE, cannot be removed from an option group, and that option group
-- cannot be removed from a DB instance once it is associated with a DB
-- instance
mogOptionGroupName :: Lens' ModifyOptionGroup Text
mogOptionGroupName = lens _mogOptionGroupName (\ s a -> s{_mogOptionGroupName = a});

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
                      _mogOptionsToInclude),
               "OptionsToRemove" =:
                 toQuery
                   (toQueryList "member" <$> _mogOptionsToRemove),
               "ApplyImmediately" =: _mogApplyImmediately,
               "OptionGroupName" =: _mogOptionGroupName]

-- | /See:/ 'modifyOptionGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mogrOptionGroup'
--
-- * 'mogrStatus'
data ModifyOptionGroupResponse = ModifyOptionGroupResponse'
    { _mogrOptionGroup :: !(Maybe OptionGroup)
    , _mogrStatus      :: !Int
    } deriving (Eq,Read,Show)

-- | 'ModifyOptionGroupResponse' smart constructor.
modifyOptionGroupResponse :: Int -> ModifyOptionGroupResponse
modifyOptionGroupResponse pStatus =
    ModifyOptionGroupResponse'
    { _mogrOptionGroup = Nothing
    , _mogrStatus = pStatus
    }

-- | FIXME: Undocumented member.
mogrOptionGroup :: Lens' ModifyOptionGroupResponse (Maybe OptionGroup)
mogrOptionGroup = lens _mogrOptionGroup (\ s a -> s{_mogrOptionGroup = a});

-- | FIXME: Undocumented member.
mogrStatus :: Lens' ModifyOptionGroupResponse Int
mogrStatus = lens _mogrStatus (\ s a -> s{_mogrStatus = a});
