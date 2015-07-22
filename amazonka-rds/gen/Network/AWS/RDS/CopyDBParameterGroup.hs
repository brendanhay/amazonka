{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CopyDBParameterGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified DB parameter group.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CopyDBParameterGroup.html>
module Network.AWS.RDS.CopyDBParameterGroup
    (
    -- * Request
      CopyDBParameterGroup
    -- ** Request constructor
    , copyDBParameterGroup
    -- ** Request lenses
    , cdpgrqTags
    , cdpgrqSourceDBParameterGroupIdentifier
    , cdpgrqTargetDBParameterGroupIdentifier
    , cdpgrqTargetDBParameterGroupDescription

    -- * Response
    , CopyDBParameterGroupResponse
    -- ** Response constructor
    , copyDBParameterGroupResponse
    -- ** Response lenses
    , cdpgrsDBParameterGroup
    , cdpgrsStatus
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
-- * 'cdpgrqTags'
--
-- * 'cdpgrqSourceDBParameterGroupIdentifier'
--
-- * 'cdpgrqTargetDBParameterGroupIdentifier'
--
-- * 'cdpgrqTargetDBParameterGroupDescription'
data CopyDBParameterGroup = CopyDBParameterGroup'
    { _cdpgrqTags                              :: !(Maybe [Tag])
    , _cdpgrqSourceDBParameterGroupIdentifier  :: !Text
    , _cdpgrqTargetDBParameterGroupIdentifier  :: !Text
    , _cdpgrqTargetDBParameterGroupDescription :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CopyDBParameterGroup' smart constructor.
copyDBParameterGroup :: Text -> Text -> Text -> CopyDBParameterGroup
copyDBParameterGroup pSourceDBParameterGroupIdentifier pTargetDBParameterGroupIdentifier pTargetDBParameterGroupDescription =
    CopyDBParameterGroup'
    { _cdpgrqTags = Nothing
    , _cdpgrqSourceDBParameterGroupIdentifier = pSourceDBParameterGroupIdentifier
    , _cdpgrqTargetDBParameterGroupIdentifier = pTargetDBParameterGroupIdentifier
    , _cdpgrqTargetDBParameterGroupDescription = pTargetDBParameterGroupDescription
    }

-- | FIXME: Undocumented member.
cdpgrqTags :: Lens' CopyDBParameterGroup [Tag]
cdpgrqTags = lens _cdpgrqTags (\ s a -> s{_cdpgrqTags = a}) . _Default;

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
cdpgrqSourceDBParameterGroupIdentifier :: Lens' CopyDBParameterGroup Text
cdpgrqSourceDBParameterGroupIdentifier = lens _cdpgrqSourceDBParameterGroupIdentifier (\ s a -> s{_cdpgrqSourceDBParameterGroupIdentifier = a});

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
cdpgrqTargetDBParameterGroupIdentifier :: Lens' CopyDBParameterGroup Text
cdpgrqTargetDBParameterGroupIdentifier = lens _cdpgrqTargetDBParameterGroupIdentifier (\ s a -> s{_cdpgrqTargetDBParameterGroupIdentifier = a});

-- | A description for the copied DB parameter group.
cdpgrqTargetDBParameterGroupDescription :: Lens' CopyDBParameterGroup Text
cdpgrqTargetDBParameterGroupDescription = lens _cdpgrqTargetDBParameterGroupDescription (\ s a -> s{_cdpgrqTargetDBParameterGroupDescription = a});

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
               "Tags" =:
                 toQuery (toQueryList "Tag" <$> _cdpgrqTags),
               "SourceDBParameterGroupIdentifier" =:
                 _cdpgrqSourceDBParameterGroupIdentifier,
               "TargetDBParameterGroupIdentifier" =:
                 _cdpgrqTargetDBParameterGroupIdentifier,
               "TargetDBParameterGroupDescription" =:
                 _cdpgrqTargetDBParameterGroupDescription]

-- | /See:/ 'copyDBParameterGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdpgrsDBParameterGroup'
--
-- * 'cdpgrsStatus'
data CopyDBParameterGroupResponse = CopyDBParameterGroupResponse'
    { _cdpgrsDBParameterGroup :: !(Maybe DBParameterGroup)
    , _cdpgrsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CopyDBParameterGroupResponse' smart constructor.
copyDBParameterGroupResponse :: Int -> CopyDBParameterGroupResponse
copyDBParameterGroupResponse pStatus =
    CopyDBParameterGroupResponse'
    { _cdpgrsDBParameterGroup = Nothing
    , _cdpgrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
cdpgrsDBParameterGroup :: Lens' CopyDBParameterGroupResponse (Maybe DBParameterGroup)
cdpgrsDBParameterGroup = lens _cdpgrsDBParameterGroup (\ s a -> s{_cdpgrsDBParameterGroup = a});

-- | FIXME: Undocumented member.
cdpgrsStatus :: Lens' CopyDBParameterGroupResponse Int
cdpgrsStatus = lens _cdpgrsStatus (\ s a -> s{_cdpgrsStatus = a});
