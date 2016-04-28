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
-- Module      : Network.AWS.RDS.CopyDBParameterGroup
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified DB parameter group.
module Network.AWS.RDS.CopyDBParameterGroup
    (
    -- * Creating a Request
      copyDBParameterGroup
    , CopyDBParameterGroup
    -- * Request Lenses
    , cdpgTags
    , cdpgSourceDBParameterGroupIdentifier
    , cdpgTargetDBParameterGroupIdentifier
    , cdpgTargetDBParameterGroupDescription

    -- * Destructuring the Response
    , copyDBParameterGroupResponse
    , CopyDBParameterGroupResponse
    -- * Response Lenses
    , cdbpgrsDBParameterGroup
    , cdbpgrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.RDS.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'copyDBParameterGroup' smart constructor.
data CopyDBParameterGroup = CopyDBParameterGroup'
    { _cdpgTags                              :: !(Maybe [Tag])
    , _cdpgSourceDBParameterGroupIdentifier  :: !Text
    , _cdpgTargetDBParameterGroupIdentifier  :: !Text
    , _cdpgTargetDBParameterGroupDescription :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CopyDBParameterGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdpgTags'
--
-- * 'cdpgSourceDBParameterGroupIdentifier'
--
-- * 'cdpgTargetDBParameterGroupIdentifier'
--
-- * 'cdpgTargetDBParameterGroupDescription'
copyDBParameterGroup
    :: Text -- ^ 'cdpgSourceDBParameterGroupIdentifier'
    -> Text -- ^ 'cdpgTargetDBParameterGroupIdentifier'
    -> Text -- ^ 'cdpgTargetDBParameterGroupDescription'
    -> CopyDBParameterGroup
copyDBParameterGroup pSourceDBParameterGroupIdentifier_ pTargetDBParameterGroupIdentifier_ pTargetDBParameterGroupDescription_ =
    CopyDBParameterGroup'
    { _cdpgTags = Nothing
    , _cdpgSourceDBParameterGroupIdentifier = pSourceDBParameterGroupIdentifier_
    , _cdpgTargetDBParameterGroupIdentifier = pTargetDBParameterGroupIdentifier_
    , _cdpgTargetDBParameterGroupDescription = pTargetDBParameterGroupDescription_
    }

-- | Undocumented member.
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
--     'my-db-param-group', or a valid ARN.
-- -   If the source DB parameter group is in a different region than the
--     copy, specify a valid DB parameter group ARN, for example
--     'arn:aws:rds:us-west-2:123456789012:pg:special-parameters'.
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
-- Example: 'my-db-parameter-group'
cdpgTargetDBParameterGroupIdentifier :: Lens' CopyDBParameterGroup Text
cdpgTargetDBParameterGroupIdentifier = lens _cdpgTargetDBParameterGroupIdentifier (\ s a -> s{_cdpgTargetDBParameterGroupIdentifier = a});

-- | A description for the copied DB parameter group.
cdpgTargetDBParameterGroupDescription :: Lens' CopyDBParameterGroup Text
cdpgTargetDBParameterGroupDescription = lens _cdpgTargetDBParameterGroupDescription (\ s a -> s{_cdpgTargetDBParameterGroupDescription = a});

instance AWSRequest CopyDBParameterGroup where
        type Rs CopyDBParameterGroup =
             CopyDBParameterGroupResponse
        request = postQuery rds
        response
          = receiveXMLWrapper "CopyDBParameterGroupResult"
              (\ s h x ->
                 CopyDBParameterGroupResponse' <$>
                   (x .@? "DBParameterGroup") <*> (pure (fromEnum s)))

instance Hashable CopyDBParameterGroup

instance NFData CopyDBParameterGroup

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
data CopyDBParameterGroupResponse = CopyDBParameterGroupResponse'
    { _cdbpgrsDBParameterGroup :: !(Maybe DBParameterGroup)
    , _cdbpgrsResponseStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CopyDBParameterGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdbpgrsDBParameterGroup'
--
-- * 'cdbpgrsResponseStatus'
copyDBParameterGroupResponse
    :: Int -- ^ 'cdbpgrsResponseStatus'
    -> CopyDBParameterGroupResponse
copyDBParameterGroupResponse pResponseStatus_ =
    CopyDBParameterGroupResponse'
    { _cdbpgrsDBParameterGroup = Nothing
    , _cdbpgrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
cdbpgrsDBParameterGroup :: Lens' CopyDBParameterGroupResponse (Maybe DBParameterGroup)
cdbpgrsDBParameterGroup = lens _cdbpgrsDBParameterGroup (\ s a -> s{_cdbpgrsDBParameterGroup = a});

-- | The response status code.
cdbpgrsResponseStatus :: Lens' CopyDBParameterGroupResponse Int
cdbpgrsResponseStatus = lens _cdbpgrsResponseStatus (\ s a -> s{_cdbpgrsResponseStatus = a});
