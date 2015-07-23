{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CopyOptionGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified option group.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CopyOptionGroup.html>
module Network.AWS.RDS.CopyOptionGroup
    (
    -- * Request
      CopyOptionGroup
    -- ** Request constructor
    , copyOptionGroup
    -- ** Request lenses
    , crqTags
    , crqSourceOptionGroupIdentifier
    , crqTargetOptionGroupIdentifier
    , crqTargetOptionGroupDescription

    -- * Response
    , CopyOptionGroupResponse
    -- ** Response constructor
    , copyOptionGroupResponse
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
-- /See:/ 'copyOptionGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crqTags'
--
-- * 'crqSourceOptionGroupIdentifier'
--
-- * 'crqTargetOptionGroupIdentifier'
--
-- * 'crqTargetOptionGroupDescription'
data CopyOptionGroup = CopyOptionGroup'
    { _crqTags                         :: !(Maybe [Tag])
    , _crqSourceOptionGroupIdentifier  :: !Text
    , _crqTargetOptionGroupIdentifier  :: !Text
    , _crqTargetOptionGroupDescription :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CopyOptionGroup' smart constructor.
copyOptionGroup :: Text -> Text -> Text -> CopyOptionGroup
copyOptionGroup pSourceOptionGroupIdentifier_ pTargetOptionGroupIdentifier_ pTargetOptionGroupDescription_ =
    CopyOptionGroup'
    { _crqTags = Nothing
    , _crqSourceOptionGroupIdentifier = pSourceOptionGroupIdentifier_
    , _crqTargetOptionGroupIdentifier = pTargetOptionGroupIdentifier_
    , _crqTargetOptionGroupDescription = pTargetOptionGroupDescription_
    }

-- | FIXME: Undocumented member.
crqTags :: Lens' CopyOptionGroup [Tag]
crqTags = lens _crqTags (\ s a -> s{_crqTags = a}) . _Default;

-- | The identifier or ARN for the source option group.
--
-- Constraints:
--
-- -   Must specify a valid option group.
-- -   If the source option group is in the same region as the copy,
--     specify a valid option group identifier, for example
--     @my-option-group@, or a valid ARN.
-- -   If the source option group is in a different region than the copy,
--     specify a valid option group ARN, for example
--     @arn:aws:rds:us-west-2:123456789012:og:special-options@.
crqSourceOptionGroupIdentifier :: Lens' CopyOptionGroup Text
crqSourceOptionGroupIdentifier = lens _crqSourceOptionGroupIdentifier (\ s a -> s{_crqSourceOptionGroupIdentifier = a});

-- | The identifier for the copied option group.
--
-- Constraints:
--
-- -   Cannot be null, empty, or blank
-- -   Must contain from 1 to 255 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
--
-- Example: @my-option-group@
crqTargetOptionGroupIdentifier :: Lens' CopyOptionGroup Text
crqTargetOptionGroupIdentifier = lens _crqTargetOptionGroupIdentifier (\ s a -> s{_crqTargetOptionGroupIdentifier = a});

-- | The description for the copied option group.
crqTargetOptionGroupDescription :: Lens' CopyOptionGroup Text
crqTargetOptionGroupDescription = lens _crqTargetOptionGroupDescription (\ s a -> s{_crqTargetOptionGroupDescription = a});

instance AWSRequest CopyOptionGroup where
        type Sv CopyOptionGroup = RDS
        type Rs CopyOptionGroup = CopyOptionGroupResponse
        request = post
        response
          = receiveXMLWrapper "CopyOptionGroupResult"
              (\ s h x ->
                 CopyOptionGroupResponse' <$>
                   (x .@? "OptionGroup") <*> (pure (fromEnum s)))

instance ToHeaders CopyOptionGroup where
        toHeaders = const mempty

instance ToPath CopyOptionGroup where
        toPath = const "/"

instance ToQuery CopyOptionGroup where
        toQuery CopyOptionGroup'{..}
          = mconcat
              ["Action" =: ("CopyOptionGroup" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Tags" =: toQuery (toQueryList "Tag" <$> _crqTags),
               "SourceOptionGroupIdentifier" =:
                 _crqSourceOptionGroupIdentifier,
               "TargetOptionGroupIdentifier" =:
                 _crqTargetOptionGroupIdentifier,
               "TargetOptionGroupDescription" =:
                 _crqTargetOptionGroupDescription]

-- | /See:/ 'copyOptionGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cogrsOptionGroup'
--
-- * 'cogrsStatus'
data CopyOptionGroupResponse = CopyOptionGroupResponse'
    { _cogrsOptionGroup :: !(Maybe OptionGroup)
    , _cogrsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CopyOptionGroupResponse' smart constructor.
copyOptionGroupResponse :: Int -> CopyOptionGroupResponse
copyOptionGroupResponse pStatus_ =
    CopyOptionGroupResponse'
    { _cogrsOptionGroup = Nothing
    , _cogrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
cogrsOptionGroup :: Lens' CopyOptionGroupResponse (Maybe OptionGroup)
cogrsOptionGroup = lens _cogrsOptionGroup (\ s a -> s{_cogrsOptionGroup = a});

-- | FIXME: Undocumented member.
cogrsStatus :: Lens' CopyOptionGroupResponse Int
cogrsStatus = lens _cogrsStatus (\ s a -> s{_cogrsStatus = a});
