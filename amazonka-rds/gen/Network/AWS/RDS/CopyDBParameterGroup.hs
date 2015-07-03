{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.RDS.CopyDBParameterGroup
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

-- | Copies the specified DB parameter group.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CopyDBParameterGroup.html>
module Network.AWS.RDS.CopyDBParameterGroup
    (
    -- * Request
      CopyDBParameterGroup
    -- ** Request constructor
    , copyDBParameterGroup
    -- ** Request lenses
    , cdpgTags
    , cdpgSourceDBParameterGroupIdentifier
    , cdpgTargetDBParameterGroupIdentifier
    , cdpgTargetDBParameterGroupDescription

    -- * Response
    , CopyDBParameterGroupResponse
    -- ** Response constructor
    , copyDBParameterGroupResponse
    -- ** Response lenses
    , copDBParameterGroup
    , copStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'copyDBParameterGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdpgTags'
--
-- * 'cdpgSourceDBParameterGroupIdentifier'
--
-- * 'cdpgTargetDBParameterGroupIdentifier'
--
-- * 'cdpgTargetDBParameterGroupDescription'
data CopyDBParameterGroup = CopyDBParameterGroup'
    { _cdpgTags                              :: !(Maybe [Tag])
    , _cdpgSourceDBParameterGroupIdentifier  :: !Text
    , _cdpgTargetDBParameterGroupIdentifier  :: !Text
    , _cdpgTargetDBParameterGroupDescription :: !Text
    } deriving (Eq,Read,Show)

-- | 'CopyDBParameterGroup' smart constructor.
copyDBParameterGroup :: Text -> Text -> Text -> CopyDBParameterGroup
copyDBParameterGroup pSourceDBParameterGroupIdentifier pTargetDBParameterGroupIdentifier pTargetDBParameterGroupDescription =
    CopyDBParameterGroup'
    { _cdpgTags = Nothing
    , _cdpgSourceDBParameterGroupIdentifier = pSourceDBParameterGroupIdentifier
    , _cdpgTargetDBParameterGroupIdentifier = pTargetDBParameterGroupIdentifier
    , _cdpgTargetDBParameterGroupDescription = pTargetDBParameterGroupDescription
    }

-- | FIXME: Undocumented member.
cdpgTags :: Lens' CopyDBParameterGroup [Tag]
cdpgTags = lens _cdpgTags (\ s a -> s{_cdpgTags = a}) . _Default;

-- | The identifier or ARN for the source DB parameter group.
--
-- Constraints:
--
-- -   Must specify a valid DB parameter group.
-- -   If the source DB parameter group is in the same region as the copy,
--     specify a valid DB parameter group identifier, for example
--     @my-db-param-group@, or a valid ARN.
-- -   If the source DB parameter group is in a different region than the
--     copy, specify a valid DB parameter group ARN, for example
--     @arn:aws:rds:us-west-2:123456789012:pg:special-parameters@.
cdpgSourceDBParameterGroupIdentifier :: Lens' CopyDBParameterGroup Text
cdpgSourceDBParameterGroupIdentifier = lens _cdpgSourceDBParameterGroupIdentifier (\ s a -> s{_cdpgSourceDBParameterGroupIdentifier = a});

-- | The identifier for the copied DB parameter group.
--
-- Constraints:
--
-- -   Cannot be null, empty, or blank
-- -   Must contain from 1 to 255 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
--
-- Example: @my-db-parameter-group@
cdpgTargetDBParameterGroupIdentifier :: Lens' CopyDBParameterGroup Text
cdpgTargetDBParameterGroupIdentifier = lens _cdpgTargetDBParameterGroupIdentifier (\ s a -> s{_cdpgTargetDBParameterGroupIdentifier = a});

-- | A description for the copied DB parameter group.
cdpgTargetDBParameterGroupDescription :: Lens' CopyDBParameterGroup Text
cdpgTargetDBParameterGroupDescription = lens _cdpgTargetDBParameterGroupDescription (\ s a -> s{_cdpgTargetDBParameterGroupDescription = a});

instance AWSRequest CopyDBParameterGroup where
        type Sv CopyDBParameterGroup = RDS
        type Rs CopyDBParameterGroup =
             CopyDBParameterGroupResponse
        request = post
        response
          = receiveXMLWrapper "CopyDBParameterGroupResult"
              (\ s h x ->
                 CopyDBParameterGroupResponse' <$>
                   (x .@? "DBParameterGroup") <*> (pure (fromEnum s)))

instance ToHeaders CopyDBParameterGroup where
        toHeaders = const mempty

instance ToPath CopyDBParameterGroup where
        toPath = const "/"

instance ToQuery CopyDBParameterGroup where
        toQuery CopyDBParameterGroup'{..}
          = mconcat
              ["Action" =: ("CopyDBParameterGroup" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Tags" =: toQuery (toQueryList "Tag" <$> _cdpgTags),
               "SourceDBParameterGroupIdentifier" =:
                 _cdpgSourceDBParameterGroupIdentifier,
               "TargetDBParameterGroupIdentifier" =:
                 _cdpgTargetDBParameterGroupIdentifier,
               "TargetDBParameterGroupDescription" =:
                 _cdpgTargetDBParameterGroupDescription]

-- | /See:/ 'copyDBParameterGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'copDBParameterGroup'
--
-- * 'copStatus'
data CopyDBParameterGroupResponse = CopyDBParameterGroupResponse'
    { _copDBParameterGroup :: !(Maybe DBParameterGroup)
    , _copStatus           :: !Int
    } deriving (Eq,Read,Show)

-- | 'CopyDBParameterGroupResponse' smart constructor.
copyDBParameterGroupResponse :: Int -> CopyDBParameterGroupResponse
copyDBParameterGroupResponse pStatus =
    CopyDBParameterGroupResponse'
    { _copDBParameterGroup = Nothing
    , _copStatus = pStatus
    }

-- | FIXME: Undocumented member.
copDBParameterGroup :: Lens' CopyDBParameterGroupResponse (Maybe DBParameterGroup)
copDBParameterGroup = lens _copDBParameterGroup (\ s a -> s{_copDBParameterGroup = a});

-- | FIXME: Undocumented member.
copStatus :: Lens' CopyDBParameterGroupResponse Int
copStatus = lens _copStatus (\ s a -> s{_copStatus = a});
