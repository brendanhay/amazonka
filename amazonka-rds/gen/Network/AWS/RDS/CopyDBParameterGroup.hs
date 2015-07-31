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
    , cdpgTags
    , cdpgSourceDBParameterGroupIdentifier
    , cdpgTargetDBParameterGroupIdentifier
    , cdpgTargetDBParameterGroupDescription

    -- * Response
    , CopyDBParameterGroupResponse
    -- ** Response constructor
    , copyDBParameterGroupResponse
    -- ** Response lenses
    , cdbpgrsDBParameterGroup
    , cdbpgrsStatus
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CopyDBParameterGroup' smart constructor.
copyDBParameterGroup :: Text -> Text -> Text -> CopyDBParameterGroup
copyDBParameterGroup pSourceDBParameterGroupIdentifier_ pTargetDBParameterGroupIdentifier_ pTargetDBParameterGroupDescription_ =
    CopyDBParameterGroup'
    { _cdpgTags = Nothing
    , _cdpgSourceDBParameterGroupIdentifier = pSourceDBParameterGroupIdentifier_
    , _cdpgTargetDBParameterGroupIdentifier = pTargetDBParameterGroupIdentifier_
    , _cdpgTargetDBParameterGroupDescription = pTargetDBParameterGroupDescription_
    }

-- | FIXME: Undocumented member.
cdpgTags :: Lens' CopyDBParameterGroup [Tag]
cdpgTags = lens _cdpgTags (\ s a -> s{_cdpgTags = a}) . _Default . _Coerce;

-- | The identifier or ARN for the source DB parameter group. For information
-- about creating an ARN, see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.html#USER_Tagging.ARN Constructing an RDS Amazon Resource Name (ARN)>.
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
        request = postQuery
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
-- * 'cdbpgrsDBParameterGroup'
--
-- * 'cdbpgrsStatus'
data CopyDBParameterGroupResponse = CopyDBParameterGroupResponse'
    { _cdbpgrsDBParameterGroup :: !(Maybe DBParameterGroup)
    , _cdbpgrsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CopyDBParameterGroupResponse' smart constructor.
copyDBParameterGroupResponse :: Int -> CopyDBParameterGroupResponse
copyDBParameterGroupResponse pStatus_ =
    CopyDBParameterGroupResponse'
    { _cdbpgrsDBParameterGroup = Nothing
    , _cdbpgrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
cdbpgrsDBParameterGroup :: Lens' CopyDBParameterGroupResponse (Maybe DBParameterGroup)
cdbpgrsDBParameterGroup = lens _cdbpgrsDBParameterGroup (\ s a -> s{_cdbpgrsDBParameterGroup = a});

-- | FIXME: Undocumented member.
cdbpgrsStatus :: Lens' CopyDBParameterGroupResponse Int
cdbpgrsStatus = lens _cdbpgrsStatus (\ s a -> s{_cdbpgrsStatus = a});
